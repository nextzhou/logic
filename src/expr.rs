pub use ops::*;

use std::rc::Rc;
use std::mem;
use std::fmt;
pub use std::ops::Not;

#[derive(Debug, Eq, Clone, PartialEq, Hash)]
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn expression_display() {
        let p = Expr::proposition('p');
        let q = Expr::proposition('q');

        assert_eq!(format!("{}", Expr::Truth::<i32>(true)), "T");
        assert_eq!(format!("{}", Expr::Truth::<i32>(false)), "F");
        assert_eq!(format!("{}", p.clone().not()), "¬p");
        assert_eq!(format!("{}", p.clone().and(q.clone())), "p∧q");
        assert_eq!(format!("{}", p.clone().or(q.clone())), "p∨q");
        assert_eq!(format!("{}", p.clone().xor(q.clone())), "p⊕q");
        assert_eq!(format!("{}", p.clone().implies(q.clone())), "p⇒q");
        assert_eq!(format!("{}", p.clone().equivalent(q.clone())), "p⇔q");
    }

    #[test]
    fn expression_priority_display() {
        let p = Expr::proposition('p');
        let q = Expr::proposition('q');
        let r = Expr::proposition('r');

        assert_eq!(format!("{}", p.clone().not().not()), "¬¬p");

        let p_and_q = p.clone().and(q.clone());
        assert_eq!(format!("{}", p_and_q.clone().not()), "¬(p∧q)");

        assert_eq!(format!("{}", p_and_q.clone().and(r.clone())), "p∧q∧r");
        assert_eq!(format!("{}", r.clone().and(p_and_q.clone())), "r∧p∧q");

        let p_or_q = p.clone().or(q.clone());
        assert_eq!(format!("{}", p_or_q.clone().or(r.clone())), "p∨q∨r");
        assert_eq!(format!("{}", r.clone().or(p_or_q.clone())), "r∨p∨q");

        assert_eq!(format!("{}", p_and_q.clone().or(r.clone())), "(p∧q)∨r");
        assert_eq!(format!("{}", r.clone().or(p_and_q.clone())), "r∨(p∧q)");

        let p_implies_q = p.clone().implies(q.clone());
        assert_eq!(format!("{}", p_implies_q.clone().not()), "¬(p⇒q)");
        assert_eq!(
            format!("{}", p_implies_q.clone().implies(r.clone())),
            "(p⇒q)⇒r"
        );
        assert_eq!(
            format!("{}", r.clone().implies(p_implies_q.clone())),
            "r⇒p⇒q"
        );

        let p_equivalent_q = p.clone().equivalent(q.clone());
        assert_eq!(format!("{}", p_equivalent_q.clone().not()), "¬(p⇔q)");
        assert_eq!(
            format!("{}", p_equivalent_q.clone().equivalent(r.clone())),
            "(p⇔q)⇔r"
        );
        assert_eq!(
            format!("{}", r.clone().equivalent(p_equivalent_q.clone())),
            "r⇔p⇔q"
        );
    }
}
