// testCompilation 11982.scala
type Head[X] = X match {
  case Tuple2[a, b] => a
}

object Unpair {
  def unpair[X <: Tuple2[Any, Any]]: Head[X] = 1 // error
  unpair[Tuple2["msg", 42]]: "msg"
}


type Head2[X] = X match {
  case Tuple2[Tuple2[a, b], Tuple2[c, d]] => a
}

object Unpair2 {
  def unpair[X <: Tuple2[Tuple2[Any, Any], Tuple2[Any, Any]]]: Head2[X] = 1 // error
  unpair[Tuple2[Tuple2["msg", 42], Tuple2[41, 40]]]: "msg"
}


type Head3[X] = X match {
  case Tuple2[a, b] => a
}

object Unpair3 {
  def unpair[X <: Tuple2[Any, Any]]: Head3[Tuple2[X, X]] = (1, 2) // error
  unpair[Tuple2["msg", 42]]: ("msg", 42)
}

trait Foo[+A, +B]

type Head4[X] = X match {
  case Foo[Foo[a, b], Foo[c, d]] => a
}

object Unpair4 {
  def unpair[X <: Foo[Any, Any]]: Head4[Foo[X, X]] = 1 // error
  unpair[Foo["msg", 42]]: "msg"
}
