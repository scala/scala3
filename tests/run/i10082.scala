object Kotlin with
  class Ctx[T](val x: T) extends AnyVal

  def fun[T, U](fn: Ctx[T] ?=> U): T => U = (x: T) => fn(using Ctx(x))
  def it[T](using ctx: Ctx[T]) = ctx.x


import Kotlin._

@main def Test =
  val res = List(1).map(fun(it + 1))
  assert(res == List(2))
