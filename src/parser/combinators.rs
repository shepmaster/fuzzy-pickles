use crate::Extent;
use super::{Master, Progress, Point, Error};
use crate::tokenizer::Token;

use peresil::combinators::{map, IntoAppend};

pub(crate) fn ext<'s, F, T>(f: F) -> impl FnOnce(&mut Master<'s>, Point<'s>) -> Progress<'s, Extent>
    where F: FnOnce(&mut Master<'s>, Point<'s>) -> Progress<'s, T>
{
    move |pm, pt| {
        let spt = pt;
        let (pt, _) = try_parse!(f(pm, pt));
        Progress::success(pt, pm.state.ex(spt, pt))
    }
}

pub(crate) fn parse_nested_until<'s, O, C>(is_open: O, is_close: C) ->
    impl Fn(&mut Master<'s>, Point<'s>) -> Progress<'s, Extent>
    where O: Fn(&Token) -> bool,
          C: Fn(&Token) -> bool,
{
    move |pm, spt| {
        let mut skipped: usize = 0;
        let mut depth: usize = 0;

        for token in spt.s {
            if token.is_end_of_file() {
                break;
            } else if is_open(token) {
                depth += 1;
            } else if is_close(token) {
                if depth == 0 {
                    break;
                } else {
                    depth -= 1;
                }
            }

            skipped += 1
        }

        let pt = spt.advance_by(skipped);
        Progress::success(pt, pm.state.ex(spt, pt))
    }
}

pub(crate) enum TailedState<P, T, E> {
    Nothing(P, E),
    ValueOnly(P, T),
    ValueAndSeparator(P, T),
}

pub(crate) fn parse_tailed<'s, F, S, T, U>(sep: S, f: F, pm: &mut Master<'s>, pt: Point<'s>) ->
    TailedState<Point<'s>, T, Error>
    where S: Fn(&mut Master<'s>, Point<'s>) -> Progress<'s, U>,
          F: Fn(&mut Master<'s>, Point<'s>) -> Progress<'s, T>,

{
    match f(pm, pt) {
        Progress { status: peresil::Status::Failure(f), point } => {
            TailedState::Nothing(point, f)
        }
        Progress { status: peresil::Status::Success(value), point } => {
            match sep(pm, point) {
                Progress { status: peresil::Status::Failure(_), point } => {
                    TailedState::ValueOnly(point, value)
                }
                Progress { status: peresil::Status::Success(_), point } => {
                    TailedState::ValueAndSeparator(point, value)
                }
            }
        }
    }
}

pub(crate) trait ImplicitSeparator {
    fn is_implicit_separator(&self) -> bool;
}

#[derive(Debug)]
pub(crate) struct Tailed<T> {
    pub values: Vec<T>,
    pub separator_count: usize,
    pub last_had_separator: bool,
}

impl<T> Default for Tailed<T> {
    fn default() -> Self {
        Tailed {
            values: Vec::new(),
            separator_count: 0,
            last_had_separator: false,
        }
    }
}

// Look for an expression that is followed by a separator. Each time
// the separator is found, another expression is attempted. Each
// expression is returned, along with the count of separators.
pub(crate) fn zero_or_more_tailed_append<'s, S, F, T, U>(append_to: Tailed<T>, sep: S, f: F) ->
    impl FnOnce(&mut Master<'s>, Point<'s>) -> Progress<'s, Tailed<T>>
    where S: Fn(&mut Master<'s>, Point<'s>) -> Progress<'s, U>,
          F: Fn(&mut Master<'s>, Point<'s>) -> Progress<'s, T>,
{
    move |pm, mut pt| {
        let mut tailed = append_to;
        loop {
            match parse_tailed(&sep, &f, pm, pt) {
                TailedState::Nothing(pt, _) => {
                    return Progress::success(pt, tailed);
                }
                TailedState::ValueOnly(pt, v) => {
                    tailed.values.push(v);
                    tailed.last_had_separator = false;
                    return Progress::success(pt, tailed);
                }
                TailedState::ValueAndSeparator(pt2, v) => {
                    pt = pt2;
                    tailed.values.push(v);
                    tailed.separator_count += 1;
                    tailed.last_had_separator = true;
                }
            }
        }
    }
}

pub(crate) fn zero_or_more_tailed<'s, S, F, T, U>(sep: S, f: F) ->
    impl FnOnce(&mut Master<'s>, Point<'s>) -> Progress<'s, Tailed<T>>
    where S: Fn(&mut Master<'s>, Point<'s>) -> Progress<'s, U>,
          F: Fn(&mut Master<'s>, Point<'s>) -> Progress<'s, T>,
{
    zero_or_more_tailed_append(Tailed::default(), sep, f)
}

pub(crate) fn zero_or_more_tailed_values<'s, S, F, T, U>(sep: S, f: F) ->
    impl FnOnce(&mut Master<'s>, Point<'s>) -> Progress<'s, Vec<T>>
    where S: Fn(&mut Master<'s>, Point<'s>) -> Progress<'s, U>,
          F: Fn(&mut Master<'s>, Point<'s>) -> Progress<'s, T>
{
    map(zero_or_more_tailed(sep, f), |t| t.values)
}

pub(crate) fn zero_or_more_tailed_values_append<'s, A, S, F, T, U>(append_to: A, sep: S, f: F) ->
    impl FnOnce(&mut Master<'s>, Point<'s>) -> Progress<'s, Vec<T>>
    where A: IntoAppend<T>,
          S: Fn(&mut Master<'s>, Point<'s>) -> Progress<'s, U>,
          F: Fn(&mut Master<'s>, Point<'s>) -> Progress<'s, T>
{
    let append_to = append_to.into();
    // TODO: How do we reset separator_count and last_had_separator?
    let tailed = Tailed { values: append_to, ..Tailed::default() };
    map(zero_or_more_tailed_append(tailed, sep, f), |t| t.values)
}

// Used after parsing a single value, but not the separator
// Foo + Bar
//    ^
pub(crate) fn zero_or_more_tailed_values_resume<'s, S, F, T, U>(sep: S, f: F) ->
    impl FnOnce(&mut Master<'s>, Point<'s>) -> Progress<'s, Vec<T>>
    where S: Fn(&mut Master<'s>, Point<'s>) -> Progress<'s, U>,
          F: Fn(&mut Master<'s>, Point<'s>) -> Progress<'s, T>,
{
    move |pm, pt| {
        let spt = pt;
        let pt = match sep(pm, pt) {
            Progress { status: peresil::Status::Failure(_), point } => {
                return Progress::success(point, Vec::new())
            }
            Progress { status: peresil::Status::Success(_), point } => {
                point
            }
        };

        match one_or_more_tailed_values(sep, f)(pm, pt) {
            Progress { status: peresil::Status::Failure(_), .. } => {
                // We parsed the separator, but not another value. Rewind to before the separator
                Progress::success(spt, Vec::new())
            }
            other => other
        }
    }
}

pub(crate) fn zero_or_more_implicitly_tailed_append<'s, S, F, T, U>(append_to: Tailed<T>, sep: S, f: F) ->
    impl FnOnce(&mut Master<'s>, Point<'s>) -> Progress<'s, Tailed<T>>
    where S: Fn(&mut Master<'s>, Point<'s>) -> Progress<'s, U>,
          F: Fn(&mut Master<'s>, Point<'s>) -> Progress<'s, T>,
          T: ImplicitSeparator,
{
    move |pm, mut pt| {
        let mut tailed = append_to;
        loop {
            match parse_tailed(&sep, &f, pm, pt) {
                TailedState::Nothing(pt, _) => {
                    return Progress::success(pt, tailed);
                }
                TailedState::ValueOnly(pt2, v) => {
                    if v.is_implicit_separator() {
                        pt = pt2;
                        tailed.values.push(v);
                        tailed.separator_count += 1;
                    } else {
                        tailed.values.push(v);
                        return Progress::success(pt2, tailed);
                    }
                    tailed.last_had_separator = false;
                }
                TailedState::ValueAndSeparator(pt2, v) => {
                    pt = pt2;
                    tailed.values.push(v);
                    tailed.separator_count += 1;
                    tailed.last_had_separator = true;
                }
            }
        }
    }
}

pub(crate) fn zero_or_more_implicitly_tailed_values<'s, S, F, T, U>(sep: S, f: F) ->
    impl FnOnce(&mut Master<'s>, Point<'s>) -> Progress<'s, Vec<T>>
    where S: Fn(&mut Master<'s>, Point<'s>) -> Progress<'s, U>,
          F: Fn(&mut Master<'s>, Point<'s>) -> Progress<'s, T>,
          T: ImplicitSeparator
{
    map(zero_or_more_implicitly_tailed_append(Tailed::default(), sep, f), |t| t.values)
}

pub(crate) fn zero_or_more_implicitly_tailed_values_terminated<'s, S, F, T, U>(sep: S, f: F) ->
    impl FnOnce(&mut Master<'s>, Point<'s>) -> Progress<'s, (Vec<T>, bool)>
    where S: Fn(&mut Master<'s>, Point<'s>) -> Progress<'s, U>,
          F: Fn(&mut Master<'s>, Point<'s>) -> Progress<'s, T>,
          T: ImplicitSeparator
{
    map(zero_or_more_implicitly_tailed_append(Tailed::default(), sep, f), |t| {
        (t.values, t.last_had_separator)
    })
}

pub(crate) fn one_or_more_tailed<'s, S, F, T, U>(sep: S, f: F) ->
    impl FnOnce(&mut Master<'s>, Point<'s>) -> Progress<'s, Tailed<T>>
    where S: Fn(&mut Master<'s>, Point<'s>) -> Progress<'s, U>,
          F: Fn(&mut Master<'s>, Point<'s>) -> Progress<'s, T>
{
    move |pm, pt| {
        let mut tailed = Tailed::default();

        match parse_tailed(&sep, &f, pm, pt) {
            TailedState::Nothing(pt, f) => {
                return Progress::failure(pt, f);
            }
            TailedState::ValueOnly(pt, v) => {
                tailed.values.push(v);
                return Progress::success(pt, tailed);
            }
            TailedState::ValueAndSeparator(pt, v) => {
                tailed.values.push(v);
                tailed.separator_count += 1;
                zero_or_more_tailed_append(tailed, sep, f)(pm, pt)
            }
        }
    }
}

pub(crate) fn one_or_more_tailed_values<'s, S, F, T, U>(sep: S, f: F) ->
    impl FnOnce(&mut Master<'s>, Point<'s>) -> Progress<'s, Vec<T>>
    where S: Fn(&mut Master<'s>, Point<'s>) -> Progress<'s, U>,
          F: Fn(&mut Master<'s>, Point<'s>) -> Progress<'s, T>
{
    map(one_or_more_tailed(sep, f), |t| t.values)
}


#[cfg(test)]
mod test {
    use super::*;
    use super::super::test_utils::*;

    // Some example parsers to play with
    use crate::parser::{comma, ident, Ident};

    fn zero_or_more_tailed_test<'s>(pm: &mut Master<'s>, pt: Point<'s>) ->
        Progress<'s, Tailed<Ident>>
    {
        zero_or_more_tailed(comma, ident)(pm, pt)
    }

    #[test]
    fn zero_or_more_tailed_with_zero() {
        let p = qp(zero_or_more_tailed_test, "");
        assert_eq!(p.values.len(), 0);
        assert_eq!(p.separator_count, 0);
    }

    #[test]
    fn zero_or_more_tailed_with_one() {
        let p = qp(zero_or_more_tailed_test, "X");
        assert_eq!(p.values.len(), 1);
        assert_eq!(p.separator_count, 0);
    }

    #[test]
    fn zero_or_more_tailed_with_one_trailing() {
        let p = qp(zero_or_more_tailed_test, "X,");
        assert_eq!(p.values.len(), 1);
        assert_eq!(p.separator_count, 1);
    }

    #[test]
    fn zero_or_more_tailed_with_two() {
        let p = qp(zero_or_more_tailed_test, "X, X");
        assert_eq!(p.values.len(), 2);
        assert_eq!(p.separator_count, 1);
    }

    #[test]
    fn zero_or_more_tailed_with_two_trailing() {
        let p = qp(zero_or_more_tailed_test, "X, X,");
        assert_eq!(p.values.len(), 2);
        assert_eq!(p.separator_count, 2);
    }

    #[test]
    fn zero_or_more_tailed_with_all_space() {
        let p = qp(zero_or_more_tailed_test, "X , X , ");
        assert_eq!(p.values.len(), 2);
        assert_eq!(p.separator_count, 2);
    }

    #[test]
    fn zero_or_more_tailed_doesnt_allow_space_separator() {
        let p = qp(zero_or_more_tailed_test, "X X");
        assert_eq!(p.values.len(), 1);
        assert_eq!(p.separator_count, 0);
    }

    fn one_or_more_tailed_test<'s>(pm: &mut Master<'s>, pt: Point<'s>) ->
        Progress<'s, Tailed<Ident>>
    {
        one_or_more_tailed(comma, ident)(pm, pt)
    }

    #[test]
    fn one_or_more_tailed_with_zero() {
        let p = parse_full(one_or_more_tailed_test, "");
        let (err_loc, errs) = unwrap_progress_err(p);
        assert_eq!(err_loc, 0);
        assert!(errs.contains(&Error::ExpectedIdent));
    }

    #[test]
    fn one_or_more_tailed_with_one() {
        let p = qp(one_or_more_tailed_test, "X");
        assert_eq!(p.values.len(), 1);
        assert_eq!(p.separator_count, 0);
    }

    #[test]
    fn one_or_more_tailed_with_one_trailing() {
        let p = qp(one_or_more_tailed_test, "X,");
        assert_eq!(p.values.len(), 1);
        assert_eq!(p.separator_count, 1);
    }

    #[test]
    fn one_or_more_tailed_with_two() {
        let p = qp(one_or_more_tailed_test, "X, X");
        assert_eq!(p.values.len(), 2);
        assert_eq!(p.separator_count, 1);
    }

    #[test]
    fn one_or_more_tailed_with_two_trailing() {
        let p = qp(one_or_more_tailed_test, "X, X,");
        assert_eq!(p.values.len(), 2);
        assert_eq!(p.separator_count, 2);
    }

    #[test]
    fn one_or_more_tailed_with_all_space() {
        let p = qp(one_or_more_tailed_test, "X , X , ");
        assert_eq!(p.values.len(), 2);
        assert_eq!(p.separator_count, 2);
    }

    #[test]
    fn one_or_more_tailed_with_two_doesnt_allow_space_separator() {
        let p = qp(one_or_more_tailed_test, "X X");
        assert_eq!(p.values.len(), 1);
        assert_eq!(p.separator_count, 0);
    }
}
