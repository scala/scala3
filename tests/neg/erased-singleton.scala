//> using options -experimental -language:experimental.erasedDefinitions

trait Sys

trait Obj {
  erased val s: Sys

  type S = s.type  // error: non final
}
