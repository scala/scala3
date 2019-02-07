
trait Sys { type X }

trait Obj {
  erased val s: Sys
  lazy val t: Sys

  type S = s.X  // error: not a legal path, since nonfinal
  type T = t.X  // error: not a legal path, since nonfinal
}