use form::DnfIdx;

pub(crate) fn major_dnf(true_idx: Vec<usize>, len: u8) -> DnfIdx {
    let idx = true_idx
        .iter()
        .map(|&state| (0..len).map(|i| (i, (state & (1 << i)) != 0)).collect())
        .collect();
    DnfIdx::new(idx)
}
