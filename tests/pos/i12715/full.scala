package repro

import compiletime.{constValue, erasedValue}

sealed trait ValidateExprInt

class And[A <: ValidateExprInt, B <: ValidateExprInt] extends ValidateExprInt
class GreaterThan[T <: Int]                           extends ValidateExprInt

object Repro:
  inline def validate[E <: ValidateExprInt](v: Int): String =
    val failMsg = validateV[E](v)
    if failMsg == "neverPass" then "neverPass"
    else "something else"

  transparent inline def validateV[E <: ValidateExprInt](v: Int): String =
    inline erasedValue[E] match
      case _: GreaterThan[t] =>
        "GreaterThan"
      case _: And[a, b] =>
        inline validateV[a](v) match
          case "" =>
            validateV[b](v)
          case other =>
            other

  @main def test(): Unit =
    println(validate[And[GreaterThan[10], GreaterThan[12]]](5))
