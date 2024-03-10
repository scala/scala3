trait A1 { type F[X <: F[_, _], Y] }  // error: cyclic reference involving type F
trait A2 { type F[X <: F, Y] }        // error: cyclic reference involving type F
trait A3 { type F[X >: F, Y] }        // error: cyclic reference involving type F
