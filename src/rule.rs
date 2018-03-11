pub trait Rule<P> {
    fn map(&self, p: &P) -> bool;
}

impl<F, P> Rule<P> for F
where
    F: Fn(&P) -> bool,
{
    fn map(&self, p: &P) -> bool {
        self(p)
    }
}
