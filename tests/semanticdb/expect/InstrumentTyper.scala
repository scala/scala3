package example

import scala.annotation.meta.param
import scala.language.existentials
import scala.language.higherKinds
import types.Test.*

class InstrumentTyper { self: AnyRef =>
  def all = List(
    Literal.int,
    Literal.long,
    Literal.float,
    Literal.double,
    Literal.nil,
    Literal.char,
    Literal.string,
    Literal.bool,
    Literal.unit,
    Literal.javaEnum,
    Literal.clazzOf,
    List()
  )
  type AnnotatedType = Int @param
  def singletonType(x: Predef.type) = ???
  final val clazzOf = classOf[Option[Int]]
}
