import scala.compiletime.constValue

type IsInt[A] = A match
  case Int => true
  case _   => false

def test =
  val x = constValue[IsInt[Int]]     // val res2: Boolean = true; works
  val y = constValue[IsInt[String]]  // val res3: Boolean = false; works

  object Foo:
    opaque type Foo = Int

  constValue[IsInt[Foo.Foo]] // error
