// TODO: generic enough to move to library?
pub(crate) fn not<P, E, S, F, T>
    (parser: F, error: E)
     -> impl FnOnce(&mut peresil::ParseMaster<P, E, S>, P) -> peresil::Progress<P, (), E>
    where F: FnOnce(&mut peresil::ParseMaster<P, E, S>, P) -> peresil::Progress<P, T, E>,
          P: peresil::Point,
          E: peresil::Recoverable,
{
    move |pm, pt| {
        match parser(pm, pt) {
            peresil::Progress { status: peresil::Status::Success(_), .. } => {
                peresil::Progress::failure(pt, error)
            }
            peresil::Progress { status: peresil::Status::Failure(_), .. } => {
                peresil::Progress::success(pt, ())
            }
        }
    }
}

// TODO: generic enough to move to library?
pub(crate) fn peek<P, E, S, F, T>
    (parser: F)
     -> impl FnOnce(&mut peresil::ParseMaster<P, E, S>, P) -> peresil::Progress<P, T, E>
    where F: FnOnce(&mut peresil::ParseMaster<P, E, S>, P) -> peresil::Progress<P, T, E>,
          P: peresil::Point,
          E: peresil::Recoverable,
{
    move |pm, pt| {
        match parser(pm, pt) {
            peresil::Progress { status: peresil::Status::Success(val), .. } => {
                peresil::Progress::success(pt, val)
            }
            peresil::Progress { status: peresil::Status::Failure(f), .. } => {
                peresil::Progress::failure(pt, f)
            }
        }
    }
}
