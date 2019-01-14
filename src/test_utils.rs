macro_rules! unwrap_as {
    ($e:expr, $p:path) => {
        match $e {
            $p(s) => s,
            _ => panic!("Not a {}: {:?}", stringify!($p), $e),
        }
    }
}

macro_rules! assert_extent {
    ($parsed:expr, $extent:expr) => (assert_eq!(crate::HasExtent::extent(&$parsed), Extent::from($extent)));
}
