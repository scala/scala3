//> using options -language:experimental.erasedDefinitions

trait Sys

trait Obj {
  erased val s: Sys

  type S = s.type  // now OK, was error: non final
}
