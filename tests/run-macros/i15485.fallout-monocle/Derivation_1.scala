// A minimisation of monocle's GenIsoSpec.scala
// which broke when fixing soundness in infering GADT constraints on refined types
trait Foo[T] { def foo: Int }
object Foo:
  import scala.deriving.*, scala.quoted.*
  inline given derived[T](using inline m: Mirror.Of[T]): Foo[T] = ${ impl('m) }
  private def impl[T](m: Expr[Mirror.Of[T]])(using Quotes, Type[T]): Expr[Foo[T]] = m match
    case '{ $m : Mirror.Product { type MirroredElemTypes =      EmptyTuple   } } => '{ FooN[T](0) }
    case '{ $m : Mirror.Product { type MirroredElemTypes = a *: EmptyTuple   } } => '{ FooN[T](1) }
    case '{ $m : Mirror.Product { type MirroredElemTypes = mirroredElemTypes } } => '{ FooN[T](9) }
  class FooN[T](val foo: Int) extends Foo[T]
