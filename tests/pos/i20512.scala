import language.experimental.namedTuples

import NamedTuple.*

trait Selector1 extends Selectable {
  type Fields = (int: Int, str: String)

  def selectDynamic(name: String)(using name.type <:< Tuple.Union[NamedTuple.Names[Fields]]) = ???
}

def test20512 = {
  val s: Selector1 = new Selector1 {}
  val int = s.int
  val str = s.str
}

trait Ctx

class Selector2 extends Selectable:
  type Fields = (bar: Int, baz: Int)
  def selectDynamic(fieldName: String)(using Ctx): Any = ???

def test22023(using Ctx) =
  val f = Selector2()
  val bar = f.bar
  val baz = f.baz