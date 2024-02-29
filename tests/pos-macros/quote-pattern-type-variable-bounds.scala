import quoted.*

def foo(using Quotes)(x: Expr[Int]) =
  x match
    case '{ type t; type u <: `t`; f[`t`, `u`] } =>
    case '{ type u <: `t`; type t; f[`t`, `u`] } =>
    case '{ type t; type u <: `t`; g[F[`t`, `u`]] } =>
    case '{ type u <: `t`; type t; g[F[`t`, `u`]] } =>

def f[T, U <: T] = ???
def g[T] = ???
type F[T, U <: T]
