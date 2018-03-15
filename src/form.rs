use std::rc::Rc;
use std::iter::Iterator;

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
    state: u64,
    rows_number: u64,
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
}

fn parse_state<T>(state: u64, propositions: &Vec<Rc<T>>) -> Vec<(Rc<T>, bool)> {
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
    }
}
