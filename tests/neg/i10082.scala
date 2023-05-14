object Kotlin:
  def it[T](using t: T) = t
  def fun[T, U](fn: T ?=> U)(x: T): U = fn(using x)

import Kotlin.{fun, it}

def Test = List(1).map(fun(it + 1)) // error