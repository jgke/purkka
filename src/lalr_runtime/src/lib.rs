pub fn coerce_panic() -> ! {
    panic!("Failed to coerce safely while parsing")
}
pub fn index_panic() -> ! {
    panic!("Found token not used in grammar")
}
