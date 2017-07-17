pub fn add(x: i32, y: i32) -> i32 { x + y }

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(add(1, 1), 2);
    }
}
