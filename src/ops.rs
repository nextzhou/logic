use expr::Expr;
use std::convert::Into;

pub trait And<T, RHS> {
    fn and(self, rhs: RHS) -> Expr<T>;
}

impl<T, LHS, RHS> And<T, RHS> for LHS
where
    RHS: Into<Expr<T>>,
    LHS: Into<Expr<T>>,
{
    fn and(self, e: RHS) -> Expr<T> {
        Expr::And(Box::new(self.into()), Box::new(e.into()))
    }
}

pub trait Or<T, RHS> {
    fn or(self, rhs: RHS) -> Expr<T>;
}

impl<T, LHS, RHS> Or<T, RHS> for LHS
where
    RHS: Into<Expr<T>>,
    LHS: Into<Expr<T>>,
{
    fn or(self, e: RHS) -> Expr<T> {
        Expr::Or(Box::new(self.into()), Box::new(e.into()))
    }
}

pub trait Xor<T, RHS> {
    fn xor(self, rhs: RHS) -> Expr<T>;
}

impl<T, LHS, RHS> Xor<T, RHS> for LHS
where
    RHS: Into<Expr<T>>,
    LHS: Into<Expr<T>>,
{
    fn xor(self, e: RHS) -> Expr<T> {
        Expr::Xor(Box::new(self.into()), Box::new(e.into()))
    }
}

pub trait Implies<T, RHS> {
    fn implies(self, rhs: RHS) -> Expr<T>;
}

impl<T, LHS, RHS> Implies<T, RHS> for LHS
where
    RHS: Into<Expr<T>>,
    LHS: Into<Expr<T>>,
{
    fn implies(self, e: RHS) -> Expr<T> {
        Expr::Implies(Box::new(self.into()), Box::new(e.into()))
    }
}

pub trait Equivalent<T, RHS> {
    fn equivalent(self, rhs: RHS) -> Expr<T>;
}

impl<T, LHS, RHS> Equivalent<T, RHS> for LHS
where
    RHS: Into<Expr<T>>,
    LHS: Into<Expr<T>>,
{
    fn equivalent(self, e: RHS) -> Expr<T> {
        Expr::Equivalent(Box::new(self.into()), Box::new(e.into()))
    }
}
