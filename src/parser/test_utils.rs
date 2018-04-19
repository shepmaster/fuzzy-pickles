use super::{Master, Point, Progress, State, Error};
use peresil;

type TestResult<T> = Result<(usize, T), (usize, Vec<Error>)>;

pub(crate) fn parse_full<'s, F, T>(f: F, s: &'s str) -> TestResult<T>
    where F: for<'a> FnOnce(&mut Master<'a>, Point<'a>) -> Progress<'a, T>
{
    let (_ws, tokens) = ::extract_whitespace(s).expect("Unable to tokenize");

    let mut pm = Master::with_state(State::new());
    let pt = Point::new(&tokens);
    let r = f(&mut pm, pt);
    match pm.finish(r) {
        peresil::Progress { status: peresil::Status::Success(v), point } => {
            Ok((point.offset, v))
        }
        peresil::Progress { status: peresil::Status::Failure(v), point } => {
            Err((point.offset, v))
        }
    }
}

pub(crate) fn qp<'s, F, T>(f: F, s: &'s str) -> T
    where F: for<'a> FnOnce(&mut Master<'a>, Point<'a>) -> Progress<'a, T>
{
    unwrap_progress(parse_full(f, s))
}

pub(crate) fn unwrap_progress<T>(p: TestResult<T>) -> T {
    match p {
        Ok((_, v)) => v,
        Err((offset, e)) => panic!("Failed parsing at token at index {}: {:?}", offset, e),
    }
}

pub(crate) fn unwrap_progress_err<T>(p: TestResult<T>) -> (usize, Vec<Error>) {
    match p {
        Ok(_) => panic!("Parsing should have failed, but it did not"),
        Err(x) => x
    }
}
