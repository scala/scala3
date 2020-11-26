type T = (=> Int) => Int
type U = (x: => Int) => Int // error, named parameter cannot be cbn since cbn dependent function types are not allowed.
type V = _ => Int
type W = (_) => Boolean
