use std::rc::Rc;
use std::iter::Iterator;
use std::ops::Not;

use ops::{And, Or};
use expr::Expr;

#[derive(Debug, Eq, PartialEq, Hash)]
pub struct TruthTable<'a, T: 'a + PartialEq> {
    pub(crate) expr: &'a Expr<T>,
    pub(crate) propositions: Vec<Rc<T>>,
}

impl<'a, T> TruthTable<'a, T>
where
    T: 'a + PartialEq,
{
    pub fn rows(&self) -> TruthTableRowIter<T> {
        TruthTableRowIter {
            truth_table: self,
            state: 0,
            rows_number: 1 << (self.propositions.len() as u64),
        }
    }

    pub fn major_dnf(&self) -> Expr<T> {
        let dnf = self.rows()
            .filter_map(|TruthTableRow { state, result }| {
                if result {
                    let minterm = state
                        .iter()
                        .map(|&(ref p, truth)| {
                            if truth {
                                Expr::Proposition(p.clone())
                            } else {
                                Expr::Proposition(p.clone()).not()
                            }
                        })
                        .fold(None, |e1: Option<Expr<T>>, e2| {
                            if let Some(e) = e1 {
                                Some(e.and(e2))
                            } else {
                                Some(e2)
                            }
                        });
                    if let Some(e) = minterm {
                        Some(e)
                    } else {
                        Some(Expr::Truth(true))
                    }
                } else {
                    None
                }
            })
            .fold(None, |e1: Option<Expr<T>>, e2| {
                if let Some(e) = e1 {
                    Some(e.or(e2))
                } else {
                    Some(e2)
                }
            });
        if let Some(e) = dnf {
            e
        } else {
            Expr::Truth(false)
        }
    }
}

#[derive(Debug, Eq, PartialEq, Hash)]
pub struct TruthTableRow<T> {
    pub state: Vec<(Rc<T>, bool)>,
    pub result: bool,
}

#[derive(Debug, Eq, PartialEq, Hash)]
pub struct TruthTableRowIter<'a, T>
where
    T: 'a + PartialEq,
{
    truth_table: &'a TruthTable<'a, T>,
    state: u32,
    rows_number: u32,
}

impl<'a, T> Clone for TruthTableRowIter<'a, T>
where
    T: 'a + PartialEq,
{
    fn clone(&self) -> Self {
        TruthTableRowIter {
            truth_table: &self.truth_table,
            state: self.state,
            rows_number: self.rows_number,
        }
    }
}

impl<'a, T> Iterator for TruthTableRowIter<'a, T>
where
    T: PartialEq,
{
    type Item = TruthTableRow<T>;
    fn next(&mut self) -> Option<Self::Item> {
        if self.state >= self.rows_number {
            None
        } else {
            let propositions = &self.truth_table.propositions;
            let state = parse_state(self.state, &self.truth_table.propositions);
            let result;
            {
                let rule = |x: &T| {
                    let pos = propositions.iter().position(|ref p| ***p == *x).unwrap();
                    state[pos].1
                };
                result = self.truth_table.expr.apply_rule(&rule);
            }
            self.state += 1;
            Some(TruthTableRow { state, result })
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (
            (self.rows_number - self.state) as usize,
            Some(self.rows_number as usize),
        )
    }

    fn count(self) -> usize {
        self.rows_number as usize
    }

    fn last(self) -> Option<Self::Item> {
        let mut iter = self;
        iter.state = iter.rows_number - 1;
        iter.next()
    }

    fn nth(&mut self, n: usize) -> Option<Self::Item> {
        self.state = n as u32;
        self.next()
    }
}

fn parse_state<T>(state: u32, propositions: &Vec<Rc<T>>) -> Vec<(Rc<T>, bool)> {
    let mut result = Vec::with_capacity(propositions.len());
    for i in 0..propositions.len() {
        if (state & (1 << i)) == 0 {
            result.push((propositions[i].clone(), false));
        } else {
            result.push((propositions[i].clone(), true));
        }
    }
    result
}

#[cfg(test)]
mod tests {
    use super::*;
    use ops::Implies;

    macro_rules! format_eq {
        ($e: expr, $s: expr) => {
            assert_eq!(format!("{}", $e), $s);
        };
    }

    #[test]
    fn truth_table() {
        let (p, q, r) = (
            &Expr::proposition('p'),
            &Expr::proposition('q'),
            &Expr::proposition('r'),
        );
        let expr = (p | q).implies(r);
        let truth_table = expr.truth_table();
        let mut rows = truth_table.rows();
        assert_eq!(rows.size_hint(), (8, Some(8)));
        macro_rules! table {
            ($($p:expr, $q: expr, $r: expr, $t: expr); *) => {
                $(assert_eq!(rows.next(), Some(TruthTableRow{
                    state: vec![(Rc::new('p'), $p), (Rc::new('q'), $q), (Rc::new('r'), $r)],
                    result: $t,
                }));)*
                assert_eq!(rows.next(), None);
            };
        }
        //       p      q     r     (p∨q)⇒r
        table!(false, false, false, true;
               true, false, false, false;
               false, true, false, false;
               true, true, false, false;
               false, false, true, true;
               true, false, true, true;
               false, true, true, true;
               true, true, true, true);
        assert_eq!(rows.size_hint(), (0, Some(8)));
    }

    #[test]
    fn empty_truth_table() {
        let p = Expr::Truth::<i32>(true);
        let truth_table = p.truth_table();
        let mut rows = truth_table.rows();
        assert_eq!(rows.size_hint(), (1, Some(1)));
        assert_eq!(
            rows.next(),
            Some(TruthTableRow {
                state: vec![],
                result: true,
            })
        );
        assert_eq!(rows.size_hint(), (0, Some(1)));
        assert_eq!(rows.next(), None);
        assert_eq!(rows.size_hint(), (0, Some(1)));
    }

    #[test]
    fn dnf() {
        let expr: Expr<i32> = Expr::truth(true) & Expr::truth(false);
        format_eq!(expr.truth_table().major_dnf(), "F");

        let expr: Expr<i32> = Expr::truth(true) | Expr::truth(false);
        format_eq!(expr.truth_table().major_dnf(), "T");

        let (p, q, r) = (
            &Expr::proposition('p'),
            &Expr::proposition('q'),
            &Expr::proposition('r'),
        );

        let expr = p & !p;
        format_eq!(expr.truth_table().major_dnf(), "F");

        let expr = (p | q).implies(r);
        format_eq!(
            expr.truth_table().major_dnf(),
            "(¬p∧¬q∧¬r)∨(¬p∧¬q∧r)∨(p∧¬q∧r)∨(¬p∧q∧r)∨(p∧q∧r)"
        );
    }
}
