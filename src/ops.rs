use expr::{Expr, SubExpr};
use std::rc::Rc;
use std::cell::RefCell;

pub trait ToSubExpr<T> {
    fn into_sub_expr(self) -> SubExpr<T>;
}

impl<T, E> ToSubExpr<T> for E where E: Into<Expr<T>> {
    fn into_sub_expr(self) -> SubExpr<T> {
        Rc::new(RefCell::new(self.into()))
    }
}

impl<'a, T> ToSubExpr<T> for &'a SubExpr<T> {
    fn into_sub_expr(self) -> SubExpr<T> {
        self.clone()
    }
}

pub trait Not<T> {
    fn not(self) -> SubExpr<T>;
}

impl<'a, T> Not<T> for &'a SubExpr<T> {
    fn not(self) -> SubExpr<T> {
        Expr::Not(self.clone()).into_sub_expr()
    }
}

impl<T> Not<T> for Expr<T> {
    fn not(self) -> SubExpr<T> {
        Expr::Not(self.into_sub_expr()).into_sub_expr()
    }
}

pub trait And<T, RHS> {
    fn and(self, rhs: RHS) -> SubExpr<T>;
}

impl<'a, T, RHS> And<T, RHS> for &'a SubExpr<T> where RHS: ToSubExpr<T> {
    fn and(self, e: RHS) -> SubExpr<T> {
        Rc::new(RefCell::new(Expr::And(self.clone(), e.into_sub_expr())))
    }
}

impl<'a, T, RHS> And<T, RHS> for Expr<T> where RHS: ToSubExpr<T> {
    fn and(self, e: RHS) -> SubExpr<T> {
        Rc::new(RefCell::new(Expr::And(self.into_sub_expr(), e.into_sub_expr())))
    }
}

pub trait Or<T, RHS> {
    fn or(self, rhs: RHS) -> SubExpr<T>;
}

impl<'a, T, RHS> Or<T, RHS> for &'a SubExpr<T> where RHS: ToSubExpr<T> {
    fn or(self, e: RHS) -> SubExpr<T> {
        Rc::new(RefCell::new(Expr::Or(self.clone(), e.into_sub_expr())))
    }
}

pub trait Xor<T, RHS> {
    fn xor(self, rhs: RHS) -> SubExpr<T>;
}

impl<'a, T, RHS> Xor<T, RHS> for &'a SubExpr<T> where RHS: ToSubExpr<T> {
    fn xor(self, e: RHS) -> SubExpr<T> {
        Rc::new(RefCell::new(Expr::Xor(self.clone(), e.into_sub_expr())))
    }
}

pub trait Implies<T, RHS> {
    fn implies(self, rhs: RHS) -> SubExpr<T>;
}

impl<'a, T, RHS> Implies<T, RHS> for &'a SubExpr<T> where RHS: ToSubExpr<T> {
    fn implies(self, e: RHS) -> SubExpr<T> {
        Rc::new(RefCell::new(Expr::Implies(self.clone(), e.into_sub_expr())))
    }
}

impl<'a, T, RHS> Implies<T, RHS> for Expr<T> where RHS: ToSubExpr<T> {
    fn implies(self, e: RHS) -> SubExpr<T> {
        Rc::new(RefCell::new(Expr::Implies(self.into_sub_expr(), e.into_sub_expr())))
    }
}

pub trait Equivalent<T, RHS> {
    fn equivalent(self, rhs: RHS) -> SubExpr<T>;
}

impl<'a, T, RHS> Equivalent<T, RHS> for &'a SubExpr<T> where RHS: ToSubExpr<T> {
    fn equivalent(self, e: RHS) -> SubExpr<T> {
        Rc::new(RefCell::new(Expr::Equivalent(self.clone(), e.into_sub_expr())))
    }
}

impl<'a, T, RHS> Equivalent<T, RHS> for Expr<T> where RHS: ToSubExpr<T> {
    fn equivalent(self, e: RHS) -> SubExpr<T> {
        Rc::new(RefCell::new(Expr::Equivalent(self.into_sub_expr(), e.into_sub_expr())))
    }
}
