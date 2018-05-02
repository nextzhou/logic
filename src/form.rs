use std::rc::Rc;
use std::iter::Iterator;

use ops::{And, Or};
use expr::Expr;
use transform::major_dnf;

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

    pub fn major_dnf(&self) -> Dnf<T> {
        let idx = self.rows()
            .enumerate()
            .filter_map(
                |(idx, TruthTableRow { result, .. })| {
                    if result {
                        Some(idx)
                    } else {
                        None
                    }
                },
            )
            .collect();
        let dnf_idx = major_dnf(idx, self.propositions.len() as u8);
        Dnf {
            idx: dnf_idx,
            propositions: self.propositions.clone(),
        }
    }

    pub fn is_tautology(&self) -> bool
    where
        T: PartialEq,
    {
        self.rows().all(|TruthTableRow { result, .. }| result)
    }

    pub fn is_contradiction(&self) -> bool
    where
        T: PartialEq,
    {
        self.rows().all(|TruthTableRow { result, .. }| !result)
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

#[derive(Debug, Eq, PartialEq, Hash)]
pub(crate) struct DnfIdx(Vec<Vec<(u8, bool)>>);

impl DnfIdx {
    pub(crate) fn new(terms_idx: Vec<Vec<(u8, bool)>>) -> Self {
        DnfIdx(terms_idx)
    }
}

#[derive(Debug, Eq, PartialEq, Hash)]
pub struct Dnf<T> {
    idx: DnfIdx,
    propositions: Vec<Rc<T>>,
}

impl<T> Dnf<T> {
    pub fn to_expr(&self) -> Expr<T> {
        self.idx
            .0
            .iter()
            .fold(None, |expr: Option<Expr<T>>, terms_idx| {
                let minterm = terms_idx
                    .iter()
                    .fold(None, |term: Option<Expr<T>>, &(idx, truth)| {
                        let mut p: Expr<T> =
                            Expr::Proposition(self.propositions[idx as usize].clone());
                        if !truth {
                            p = !p;
                        }
                        if let Some(t) = term {
                            Some(t.and(p))
                        } else {
                            Some(p)
                        }
                    })
                    .unwrap_or(Expr::truth(true));
                if let Some(e) = expr {
                    Some(e.or(minterm))
                } else {
                    Some(minterm)
                }
            })
            .unwrap_or(Expr::truth(false))
    }
}

impl<T> Into<Expr<T>> for Dnf<T> {
    fn into(self) -> Expr<T> {
        self.to_expr()
    }
}

impl<'a, T> Into<Expr<T>> for &'a Dnf<T> {
    fn into(self) -> Expr<T> {
        self.to_expr()
    }
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
        let truth_table = expr.truth_table().unwrap();
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
        let truth_table = p.truth_table().unwrap();
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
        format_eq!(expr.truth_table().unwrap().major_dnf().to_expr(), "F");

        let expr: Expr<i32> = Expr::truth(true) | Expr::truth(false);
        format_eq!(expr.truth_table().unwrap().major_dnf().to_expr(), "T");

        let (p, q, r) = (
            &Expr::proposition('p'),
            &Expr::proposition('q'),
            &Expr::proposition('r'),
        );

        let expr = p & !p;
        format_eq!(expr.truth_table().unwrap().major_dnf().to_expr(), "F");

        let expr = (p | q).implies(r);
        format_eq!(
            expr.truth_table().unwrap().major_dnf().to_expr(),
            "(¬p∧¬q∧¬r)∨(¬p∧¬q∧r)∨(p∧¬q∧r)∨(¬p∧q∧r)∨(p∧q∧r)"
        );
    }
}
