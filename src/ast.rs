#[derive(Clone, PartialEq)]
pub struct Puntuated<T, P> {
    pub items: Vec<(T, P)>,
    pub last: Option<T>,
}

