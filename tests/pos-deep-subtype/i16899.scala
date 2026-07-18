//> using options -Werror

sealed trait Unset

def foo(v: Unset|Option[Int]): Unit = v match
  case v: Unset => ()
  case v: Option[Int] => ()
