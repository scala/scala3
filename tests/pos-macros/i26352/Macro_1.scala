// https://github.com/scala/scala3/issues/26352
import scala.quoted.*

class Box[I]

abstract class Outer:
  self =>
  object inner:
    transparent inline def make: Box[?] = ${ Outer.makeMacro[self.type] }

object Outer:
  def makeMacro[I: Type](using Quotes): Expr[Box[?]] = '{ new Box[I]() }
