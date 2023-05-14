class Type

abstract class TypeMap:
  thisMap =>
  def apply(tp: Type): Type
  def andThen(f: Type => Type): TypeMap = new TypeMap:
    def apply(tp: Type) = f(thisMap(tp))

class Test:
  val typeMap = new TypeMap:
    def apply(tp: Type) = tp
  println(typeMap)
  val n = 10
