pub use ops::*;
use rule::Rule;

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

    pub fn apply_rule<R>(&self, rule: &R) -> bool
    where
        R: Rule<T>,
    {
        match *self {
            Expr::Truth(t) => t,
            Expr::Proposition(ref p) => rule.map(p),
            Expr::Not(ref e) => !(e.apply_rule(rule)),
            Expr::And(ref e1, ref e2) => e1.apply_rule(rule) && e2.apply_rule(rule),
            Expr::Or(ref e1, ref e2) => e1.apply_rule(rule) || e2.apply_rule(rule),
            Expr::Xor(ref e1, ref e2) => e1.apply_rule(rule) ^ e2.apply_rule(rule),
            Expr::Implies(ref e1, ref e2) => {
                let (t1, t2) = (e1.apply_rule(rule), e2.apply_rule(rule));
                !(t1 && !t2)
            }
            Expr::Equivalent(ref e1, ref e2) => e1.apply_rule(rule) == e2.apply_rule(rule),
        }
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

impl<'a, T> Into<Expr<T>> for &'a Expr<T> {
    fn into(self) -> Expr<T> {
        self.clone()
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

impl<'a, T> Not for &'a Expr<T> {
    type Output = Expr<T>;
    fn not(self) -> Self::Output {
        self.clone().not()
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

impl<'a, RHS, T> BitAnd<RHS> for &'a Expr<T>
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

impl<'a, RHS, T> BitOr<RHS> for &'a Expr<T>
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

impl<'a, RHS, T> BitXor<RHS> for &'a Expr<T>
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
    fn apply_rule() {
        let (p, q) = (&Expr::proposition('p'), &Expr::proposition('q'));
        let rule1 = |p: &char| if *p == 'p' { true } else { false }; // p is true, q is false
        let rule2 = |p: &char| if *p == 'q' { true } else { false }; // p is false, q is false
        let rule3 = |_: &char| true; // always returns true
        let rule4 = |_: &char| false; // always returns false

        macro_rules! rule_test {
            ($expr: expr, $r1: expr, $r2: expr, $r3: expr, $r4: expr) => {
                assert_eq!(($expr).apply_rule(&rule1), $r1);
                assert_eq!(($expr).apply_rule(&rule2), $r2);
                assert_eq!(($expr).apply_rule(&rule3), $r3);
                assert_eq!(($expr).apply_rule(&rule4), $r4);
            };
            ($e: expr) => {
                // assert e1 always is true
                assert!(($e).apply_rule(&rule1));
                assert!(($e).apply_rule(&rule2));
                assert!(($e).apply_rule(&rule3));
                assert!(($e).apply_rule(&rule4));
            };
            ($e1: expr, $e2: expr) => {
                // assert e1 always equals e2
                assert_eq!(($e1).apply_rule(&rule1), ($e2).apply_rule(&rule1));
                assert_eq!(($e1).apply_rule(&rule2), ($e2).apply_rule(&rule2));
                assert_eq!(($e1).apply_rule(&rule3), ($e2).apply_rule(&rule3));
                assert_eq!(($e1).apply_rule(&rule4), ($e2).apply_rule(&rule4));
            };
        }

        rule_test!(p, p);
        rule_test!(q, q);

        rule_test!(!!p, p);
        rule_test!(!!!p, !p);

        rule_test!(p, p & p);
        rule_test!(p, p | p);

        rule_test!(!(p & !p));
        rule_test!(p | !p);
        rule_test!(p ^ !p);

        rule_test!(p & q, q & p);
        rule_test!(p | q, q | p);
        rule_test!(p ^ q, q ^ p);
        rule_test!(p.equivalent(q), q.equivalent(p));

        rule_test!(Expr::truth(false).implies(p));
        rule_test!(Expr::truth(true).implies(p), p);

        rule_test!(p, true, false, true, false);
        rule_test!(!p, false, true, false, true);

        rule_test!(q, false, true, true, false);
        rule_test!(!q, true, false, false, true);

        rule_test!(p & q, false, false, true, false);
        rule_test!(p | q, true, true, true, false);
        rule_test!(p ^ q, true, true, false, false);

        rule_test!(p.implies(q), false, true, true, true);
        rule_test!(q.implies(p), true, false, true, true);

        rule_test!(p.equivalent(q), false, false, true, true);
        rule_test!(q.equivalent(p), false, false, true, true);
    }

    #[test]
    fn operation() {
        let (p, q) = (&Expr::proposition('p'), &Expr::proposition('q'));
        assert_eq!(p.not(), !p);
        assert_eq!(p.and(q), p & q);
        assert_eq!(p.or(q), p | q);
        assert_eq!(p.xor(q), p ^ q);
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
        let p = &Expr::proposition('p');
        let q = &Expr::proposition('q');

        format_eq!(Expr::Truth::<i32>(true), "T");
        format_eq!(Expr::Truth::<i32>(false), "F");
        format_eq!(p.not(), "¬p");
        format_eq!(p.and(q), "p∧q");
        format_eq!(p.or(q), "p∨q");
        format_eq!(p.xor(q), "p⊕q");
        format_eq!(p.implies(q), "p⇒q");
        format_eq!(p.equivalent(q), "p⇔q");
    }

    #[test]
    fn expression_priority_display() {
        let p = &Expr::proposition('p');
        let q = &Expr::proposition('q');
        let r = &Expr::proposition('r');

        format_eq!(p.not().not(), "¬¬p");

        let p_and_q = &(p.and(q));
        format_eq!(p_and_q.not(), "¬(p∧q)");

        format_eq!(p_and_q.and(r), "p∧q∧r");
        format_eq!(r.and(p_and_q), "r∧p∧q");

        let p_or_q = &(p.or(q));
        format_eq!(p_or_q.or(r), "p∨q∨r");
        format_eq!(r.or(p_or_q), "r∨p∨q");

        format_eq!(p_and_q.or(r), "(p∧q)∨r");
        format_eq!(r.or(p_and_q), "r∨(p∧q)");

        let p_implies_q = &(p.implies(q));
        format_eq!(p_implies_q.not(), "¬(p⇒q)");
        format_eq!(p_implies_q.implies(r), "(p⇒q)⇒r");
        format_eq!(r.implies(p_implies_q), "r⇒p⇒q");

        let p_equivalent_q = &(p.equivalent(q));
        format_eq!(p_equivalent_q.not(), "¬(p⇔q)");
        format_eq!(p_equivalent_q.equivalent(r), "(p⇔q)⇔r");
        format_eq!(r.equivalent(p_equivalent_q), "r⇔p⇔q");
    }
}
