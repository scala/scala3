def g(cond: Boolean) = if cond then true else false
def f(x: Any) =
  x match
    case s: String if g:
      s.length > 4
    => s
    case x => x.toString
def fAligned(x: Any) =
  x match
  case s: String if g:
    s.length > 4
  => s
  case x => x.toString
def fNormalized(x: Any) =
  x match
  case s: String if g(s.length > 4) => s
  case x => x.toString

@main def main =
  println:
    f("hello, world")

def failure =
  List(42)
    .collect {
      case i if
        (i > 27) =>
          -1
      }

def good =
  List(42)
    .collect {
      case i
      if (i > 27) =>
          -1
      }

class C:

  def list = List[Int | Double](42, 3.14)
  val n = 27

  def test = //2
    list.map: //4
      case i: Int => //6 //6 indent then case
        n match //8
        case j: Int //8
        if list.exists: k =>
          list.exists(_ == j && j == k) //10
        =>
          1
        case _ => 2
      case _ => 3
