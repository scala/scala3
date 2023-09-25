type Untyped = Type | Null

class Type
abstract class SearchFailureType extends Type

abstract class Tree[+T <: Untyped]:
  def tpe: T = null.asInstanceOf[T]

class SearchFailureIdent[+T <: Untyped] extends Tree[T]

class Test_i17230_bootstrap:
  def t1(arg: Tree[Type]) = arg match
    case arg: SearchFailureIdent[?] => arg.tpe match
      case x: SearchFailureType   =>
      case _                      =>
    case _ =>
