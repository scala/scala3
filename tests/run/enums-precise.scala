enum NonEmptyList[+T]:
  case Many[+U](head: U, tail: NonEmptyList[U]) extends NonEmptyList[U]
  case One [+U](value: U)                       extends NonEmptyList[U]

enum Ast:
  case Binding(name: String, tpe: String)
  case Lambda(args: NonEmptyList[Binding], rhs: Ast) // reference to another case of the enum
  case Ident(name: String)
  case Apply(fn: Ast, args: NonEmptyList[Ast])

import NonEmptyList.*
import Ast.*

// This example showcases the widening when inferring enum case types.
// With scala 2 case class hierarchies, if One.apply(1) returns One[Int] and Many.apply(2, One(3)) returns Many[Int]
// then the `foldRight` expression below would complain that Many[Binding] is not One[Binding]. With Scala 3 enums,
// .apply on the companion returns the precise class, but type inference will widen to NonEmptyList[Binding] unless
// the precise class is expected.
def Bindings(arg: (String, String), args: (String, String)*): NonEmptyList[Binding] =
  def Bind(arg: (String, String)): Binding =
    val (name, tpe) = arg
    Binding(name, tpe)

  args.foldRight(One[Binding](Bind(arg)))((arg, acc) => Many(Bind(arg), acc))

@main def Test: Unit =
  val OneOfOne: One[1] = One[1](1)
  val True = Lambda(Bindings("x" -> "T", "y" -> "T"), Ident("x"))
  val Const = Lambda(One(Binding("x", "T")), Lambda(One(Binding("y", "U")), Ident("x"))) // precise type is forwarded

  assert(OneOfOne.value == 1)
