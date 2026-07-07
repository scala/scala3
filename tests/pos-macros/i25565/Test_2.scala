object TestCase1:
  enum Foo:
    case A
    case B
  enum Bar:
    case A
    case B

  val values = Repro.singletonValues[(Foo, Bar)]

def TestCase2 =
  type Foo = "a" | "b"
  type Bar = "x" | "y"

  val values = Repro.singletonValues[(Foo, Bar)]

case class MyPair[A, B](first: A, second: B)

object TestCase3:
  enum X:
    case P
    case Q
  enum Y:
    case R
    case S

  val values = Repro.singletonValues[MyPair[X, Y]]
