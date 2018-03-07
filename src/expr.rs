use std::rc::Rc;
use std::cell::RefCell;
use std::mem;
use std::fmt;

#[derive(Debug, Eq, Clone, PartialEq)]
pub enum Expr<T> {
    Truth(bool),
    Proposition(T),
    Not(Rc<RefCell<Expr<T>>>),
    And(Rc<RefCell<Expr<T>>>, Rc<RefCell<Expr<T>>>),
    Or(Rc<RefCell<Expr<T>>>, Rc<RefCell<Expr<T>>>),
    Xor(Rc<RefCell<Expr<T>>>, Rc<RefCell<Expr<T>>>),
    Implies(Rc<RefCell<Expr<T>>>, Rc<RefCell<Expr<T>>>),
    Equivalent(Rc<RefCell<Expr<T>>>, Rc<RefCell<Expr<T>>>),
}

impl<T> Expr<T> {
    pub fn truth(t: bool) -> Self {
        Expr::Truth(t)
    }

    pub fn proposition(p: T) -> Self {
        Expr::Proposition(p)
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
                if e.borrow().has_obvious_priority_over(self)
                    || e.borrow().priority() == self.priority()
                {
                    write!(f, "¬{}", e.borrow())
                } else {
                    write!(f, "¬({})", e.borrow())
                }
            }
            Expr::And(ref e1, ref e2) => {
                if e1.borrow().has_obvious_priority_over(self)
                    || e1.borrow().has_same_operation(self)
                {
                    write!(f, "{}∧", e1.borrow())
                } else {
                    write!(f, "({})∧", e1.borrow())
                }?;
                if e2.borrow().has_obvious_priority_over(self)
                    || e2.borrow().has_same_operation(self)
                {
                    write!(f, "{}", e2.borrow())
                } else {
                    write!(f, "({})", e2.borrow())
                }
            }
            Expr::Or(ref e1, ref e2) => {
                if e1.borrow().has_obvious_priority_over(self)
                    || e1.borrow().has_same_operation(self)
                {
                    write!(f, "{}∨", e1.borrow())
                } else {
                    write!(f, "({})∨", e1.borrow())
                }?;
                if e2.borrow().has_obvious_priority_over(self)
                    || e2.borrow().has_same_operation(self)
                {
                    write!(f, "{}", e2.borrow())
                } else {
                    write!(f, "({})", e2.borrow())
                }
            }
            Expr::Xor(ref e1, ref e2) => {
                if e1.borrow().has_obvious_priority_over(self)
                    || e1.borrow().has_same_operation(self)
                {
                    write!(f, "{}⊕", e1.borrow())
                } else {
                    write!(f, "({})⊕", e1.borrow())
                }?;
                if e2.borrow().has_obvious_priority_over(self)
                    || e2.borrow().has_same_operation(self)
                {
                    write!(f, "{}", e2.borrow())
                } else {
                    write!(f, "({})", e2.borrow())
                }
            }
            Expr::Implies(ref e1, ref e2) => {
                if e1.borrow().has_obvious_priority_over(self) {
                    write!(f, "{}⇒", e1.borrow())
                } else {
                    write!(f, "({})⇒", e1.borrow())
                }?;
                if e2.borrow().has_obvious_priority_over(self)
                    || e2.borrow().has_same_operation(self)
                {
                    write!(f, "{}", e2.borrow())
                } else {
                    write!(f, "({})", e2.borrow())
                }
            }
            Expr::Equivalent(ref e1, ref e2) => {
                if e1.borrow().has_obvious_priority_over(self) {
                    write!(f, "{}⇔", e1.borrow())
                } else {
                    write!(f, "({})⇔", e1.borrow())
                }?;
                if e2.borrow().has_obvious_priority_over(self)
                    || e2.borrow().has_same_operation(self)
                {
                    write!(f, "{}", e2.borrow())
                } else {
                    write!(f, "({})", e2.borrow())
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn expression_display() {
        let p = Rc::new(RefCell::new(Expr::Proposition('p')));
        let q = Rc::new(RefCell::new(Expr::Proposition('q')));

        assert_eq!(format!("{}", Expr::Truth::<i32>(true)), "T");
        assert_eq!(format!("{}", Expr::Truth::<i32>(false)), "F");
        assert_eq!(format!("{}", Expr::Not(p.clone())), "¬p");
        assert_eq!(format!("{}", Expr::And(p.clone(), q.clone())), "p∧q");
        assert_eq!(format!("{}", Expr::Or(p.clone(), q.clone())), "p∨q");
        assert_eq!(format!("{}", Expr::Xor(p.clone(), q.clone())), "p⊕q");
        assert_eq!(format!("{}", Expr::Implies(p.clone(), q.clone())), "p⇒q");
        assert_eq!(
            format!("{}", Expr::Equivalent(p.clone(), q.clone())),
            "p⇔q"
        );
    }

    #[test]
    fn expression_priority_display() {
        let p = Rc::new(RefCell::new(Expr::Proposition('p')));
        let q = Rc::new(RefCell::new(Expr::Proposition('q')));
        let r = Rc::new(RefCell::new(Expr::Proposition('r')));

        let not_p = Rc::new(RefCell::new(Expr::Not(p.clone())));
        assert_eq!(format!("{}", Expr::Not(not_p)), "¬¬p");
        let p_and_q = Rc::new(RefCell::new(Expr::And(p.clone(), q.clone())));
        assert_eq!(format!("{}", Expr::Not(p_and_q.clone())), "¬(p∧q)");

        assert_eq!(
            format!("{}", Expr::And(p_and_q.clone(), r.clone())),
            "p∧q∧r"
        );
        assert_eq!(
            format!("{}", Expr::And(r.clone(), p_and_q.clone())),
            "r∧p∧q"
        );

        let p_or_q = Rc::new(RefCell::new(Expr::Or(p.clone(), q.clone())));
        assert_eq!(
            format!("{}", Expr::Or(p_or_q.clone(), r.clone())),
            "p∨q∨r"
        );
        assert_eq!(
            format!("{}", Expr::Or(r.clone(), p_or_q.clone())),
            "r∨p∨q"
        );

        assert_eq!(
            format!("{}", Expr::Or(p_and_q.clone(), r.clone())),
            "(p∧q)∨r"
        );
        assert_eq!(
            format!("{}", Expr::Or(r.clone(), p_and_q.clone())),
            "r∨(p∧q)"
        );

        let p_implies_q = Rc::new(RefCell::new(Expr::Implies(p.clone(), q.clone())));
        assert_eq!(format!("{}", Expr::Not(p_implies_q.clone())), "¬(p⇒q)");
        assert_eq!(
            format!("{}", Expr::Implies(p_implies_q.clone(), r.clone())),
            "(p⇒q)⇒r"
        );
        assert_eq!(
            format!("{}", Expr::Implies(r.clone(), p_implies_q.clone())),
            "r⇒p⇒q"
        );

        let p_equivalent_q = Rc::new(RefCell::new(Expr::Equivalent(p.clone(), q.clone())));
        assert_eq!(
            format!("{}", Expr::Not(p_equivalent_q.clone())),
            "¬(p⇔q)"
        );
        assert_eq!(
            format!("{}", Expr::Equivalent(p_equivalent_q.clone(), r.clone())),
            "(p⇔q)⇔r"
        );
        assert_eq!(
            format!("{}", Expr::Equivalent(r.clone(), p_equivalent_q.clone())),
            "r⇔p⇔q"
        );
    }
}
