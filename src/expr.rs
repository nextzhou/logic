pub use ops::*;

use std::rc::Rc;
use std::mem;
use std::fmt;
use std::ops::{BitAnd, BitOr, BitXor, Not};

#[derive(Debug, Eq, PartialEq, Hash)]
pub enum Expr<T> {
    Truth(bool),
    Proposition(Rc<T>),
    Not(Box<Expr<T>>),
    And(Box<Expr<T>>, Box<Expr<T>>),
    Or(Box<Expr<T>>, Box<Expr<T>>),
    Xor(Box<Expr<T>>, Box<Expr<T>>),
    Implies(Box<Expr<T>>, Box<Expr<T>>),
    Equivalent(Box<Expr<T>>, Box<Expr<T>>),
}

impl<T> Expr<T> {
    pub fn truth(t: bool) -> Self {
        Expr::Truth(t)
    }

    pub fn proposition(p: T) -> Self {
        Expr::Proposition(Rc::new(p))
    }

    pub fn has_same_operation(&self, e: &Expr<T>) -> bool {
        mem::discriminant(self) == mem::discriminant(e)
    }

    fn priority(&self) -> u8 {
        match *self {
            Expr::Truth(..) | Expr::Proposition(..) => 0,
            Expr::Not(..) => 1,
            Expr::And(..) => 2,
            Expr::Or(..) => 3,
            Expr::Xor(..) => 4,
            Expr::Implies(..) => 5,
            Expr::Equivalent(..) => 6,
        }
    }

    fn vague_priority(&self) -> u8 {
        match *self {
            Expr::Truth(..) | Expr::Proposition(..) => 0,
            Expr::Not(..) => 1,
            Expr::And(..) | Expr::Or(..) | Expr::Xor(..) => 2,
            Expr::Implies(..) | Expr::Equivalent(..) => 3,
        }
    }

    fn has_obvious_priority_over(&self, e: &Expr<T>) -> bool {
        self.vague_priority() < e.vague_priority()
    }
}

// implement Clone for Expr<T> no matter whether T is cloned
impl<T> Clone for Expr<T> {
    fn clone(&self) -> Self {
        match *self {
            Expr::Truth(t) => Expr::Truth(t),
            Expr::Proposition(ref p) => Expr::Proposition(p.clone()),
            Expr::Not(ref e) => Expr::Not(e.clone()),
            Expr::And(ref e1, ref e2) => Expr::And(e1.clone(), e2.clone()),
            Expr::Or(ref e1, ref e2) => Expr::Or(e1.clone(), e2.clone()),
            Expr::Xor(ref e1, ref e2) => Expr::Xor(e1.clone(), e2.clone()),
            Expr::Implies(ref e1, ref e2) => Expr::Implies(e1.clone(), e2.clone()),
            Expr::Equivalent(ref e1, ref e2) => Expr::Equivalent(e1.clone(), e2.clone()),
        }
    }
}

impl<T> fmt::Display for Expr<T>
where
    T: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Expr::Truth(t) => write!(f, "{}", if t { 'T' } else { 'F' }),
            Expr::Proposition(ref p) => write!(f, "{}", p),
            Expr::Not(ref e) => {
                if e.has_obvious_priority_over(self) || e.priority() == self.priority() {
                    write!(f, "¬{}", e)
                } else {
                    write!(f, "¬({})", e)
                }
            }
            Expr::And(ref e1, ref e2) => {
                if e1.has_obvious_priority_over(self) || e1.has_same_operation(self) {
                    write!(f, "{}∧", e1)
                } else {
                    write!(f, "({})∧", e1)
                }?;
                if e2.has_obvious_priority_over(self) || e2.has_same_operation(self) {
                    write!(f, "{}", e2)
                } else {
                    write!(f, "({})", e2)
                }
            }
            Expr::Or(ref e1, ref e2) => {
                if e1.has_obvious_priority_over(self) || e1.has_same_operation(self) {
                    write!(f, "{}∨", e1)
                } else {
                    write!(f, "({})∨", e1)
                }?;
                if e2.has_obvious_priority_over(self) || e2.has_same_operation(self) {
                    write!(f, "{}", e2)
                } else {
                    write!(f, "({})", e2)
                }
            }
            Expr::Xor(ref e1, ref e2) => {
                if e1.has_obvious_priority_over(self) || e1.has_same_operation(self) {
                    write!(f, "{}⊕", e1)
                } else {
                    write!(f, "({})⊕", e1)
                }?;
                if e2.has_obvious_priority_over(self) || e2.has_same_operation(self) {
                    write!(f, "{}", e2)
                } else {
                    write!(f, "({})", e2)
                }
            }
            Expr::Implies(ref e1, ref e2) => {
                if e1.has_obvious_priority_over(self) {
                    write!(f, "{}⇒", e1)
                } else {
                    write!(f, "({})⇒", e1)
                }?;
                if e2.has_obvious_priority_over(self) || e2.has_same_operation(self) {
                    write!(f, "{}", e2)
                } else {
                    write!(f, "({})", e2)
                }
            }
            Expr::Equivalent(ref e1, ref e2) => {
                if e1.has_obvious_priority_over(self) {
                    write!(f, "{}⇔", e1)
                } else {
                    write!(f, "({})⇔", e1)
                }?;
                if e2.has_obvious_priority_over(self) || e2.has_same_operation(self) {
                    write!(f, "{}", e2)
                } else {
                    write!(f, "({})", e2)
                }
            }
        }
    }
}

impl<T> Not for Expr<T> {
    type Output = Expr<T>;
    fn not(self) -> Self::Output {
        Expr::Not(Box::new(self))
    }
}

impl<RHS, T> BitAnd<RHS> for Expr<T>
where
    RHS: Into<Expr<T>>,
{
    type Output = Expr<T>;
    fn bitand(self, e: RHS) -> Self::Output {
        self.and(e.into())
    }
}

impl<RHS, T> BitOr<RHS> for Expr<T>
where
    RHS: Into<Expr<T>>,
{
    type Output = Expr<T>;
    fn bitor(self, e: RHS) -> Self::Output {
        self.or(e.into())
    }
}

impl<RHS, T> BitXor<RHS> for Expr<T>
where
    RHS: Into<Expr<T>>,
{
    type Output = Expr<T>;
    fn bitxor(self, e: RHS) -> Self::Output {
        self.xor(e.into())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn operation() {
        let (p, q) = (Expr::proposition('p'), Expr::proposition('q'));
        assert_eq!(p.clone().not(), !p.clone());
        assert_eq!(p.clone().and(q.clone()), p.clone() & q.clone());
        assert_eq!(p.clone().or(q.clone()), p.clone() | q.clone());
        assert_eq!(p.clone().xor(q.clone()), p.clone() ^ q.clone());
    }

    #[test]
    fn clone() {
        #[derive(Debug, Eq, PartialEq)]
        struct A(i32);
        let e1 = Expr::proposition(A(1));
        let e2 = e1.clone();
        assert_eq!(e1, e2);
    }

    macro_rules! format_eq {
        ($e: expr, $s: expr) => {
            assert_eq!(format!("{}", $e), $s);
        };
    }

    #[test]
    fn expression_display() {
        let p = Expr::proposition('p');
        let q = Expr::proposition('q');

        format_eq!(Expr::Truth::<i32>(true), "T");
        format_eq!(Expr::Truth::<i32>(false), "F");
        format_eq!(p.clone().not(), "¬p");
        format_eq!(p.clone().and(q.clone()), "p∧q");
        format_eq!(p.clone().or(q.clone()), "p∨q");
        format_eq!(p.clone().xor(q.clone()), "p⊕q");
        format_eq!(p.clone().implies(q.clone()), "p⇒q");
        format_eq!(p.clone().equivalent(q.clone()), "p⇔q");
    }

    #[test]
    fn expression_priority_display() {
        let p = Expr::proposition('p');
        let q = Expr::proposition('q');
        let r = Expr::proposition('r');

        format_eq!(p.clone().not().not(), "¬¬p");

        let p_and_q = p.clone().and(q.clone());
        format_eq!(p_and_q.clone().not(), "¬(p∧q)");

        format_eq!(p_and_q.clone().and(r.clone()), "p∧q∧r");
        format_eq!(r.clone().and(p_and_q.clone()), "r∧p∧q");

        let p_or_q = p.clone().or(q.clone());
        format_eq!(p_or_q.clone().or(r.clone()), "p∨q∨r");
        format_eq!(r.clone().or(p_or_q.clone()), "r∨p∨q");

        format_eq!(p_and_q.clone().or(r.clone()), "(p∧q)∨r");
        format_eq!(r.clone().or(p_and_q.clone()), "r∨(p∧q)");

        let p_implies_q = p.clone().implies(q.clone());
        format_eq!(p_implies_q.clone().not(), "¬(p⇒q)");
        format_eq!(p_implies_q.clone().implies(r.clone()), "(p⇒q)⇒r");
        format_eq!(r.clone().implies(p_implies_q.clone()), "r⇒p⇒q");

        let p_equivalent_q = p.clone().equivalent(q.clone());
        format_eq!(p_equivalent_q.clone().not(), "¬(p⇔q)");
        format_eq!(p_equivalent_q.clone().equivalent(r.clone()), "(p⇔q)⇔r");
        format_eq!(r.clone().equivalent(p_equivalent_q.clone()), "r⇔p⇔q");
    }
}
