class Parent { class E }

object ChildA extends Parent

object ChildB extends Parent

class Printer[C <: Parent](val child: C):
  def print22(e: child.E): String = ""

def test =
  Printer(ChildA).print22(new ChildA.E) // does not work

  //Printer[ChildA.type](ChildA).print22(new ChildA.E) // works
  //val p = Printer(ChildA); p.print22(new ChildA.E) // works
