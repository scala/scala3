sealed trait Unset

def foo(v: Unset|Option[Int]): Unit = v match
  case v: Unset => ()
  case v: Option[Int] => () // ok

def foo2(v: Unset|Option[Int]): Unit = v match
  case _: Unset | _: Option[Int] => () // ok

def foo3(v: Unset|Option[?]): Unit = v match
  case v: Unset => ()
  case v: Option[Int] => () // warn

def foo4(v: Unset|Option[?]): Unit = v match
  case v: Unset => ()
  case v @ (_: Option[Int] | _: Option[String]) => () // warn // warn
