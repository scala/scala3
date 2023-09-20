import scala.quoted.*

def empty[K <: AnyKind : Type](using Quotes): Type[?] =
  Type.of[K] match
    case '[type t; `t`] => Type.of[t] // error
    case '[type f[X]; `f`] => Type.of[f] // error
    case '[type f[X <: Int, Y]; `f`] => Type.of[f] // error
    case '[type k <: AnyKind; `k` ] => Type.of[k] // error
