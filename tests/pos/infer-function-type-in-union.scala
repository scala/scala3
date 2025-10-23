
def f[T](x: T): T = ???
def f2[T](x: T | T): T = ???
def f3[T](x: T | Null): T = ???
def f4[T](x: Int | T): T = ???

trait MyOption[+T]

object MyOption:
  def apply[T](x: T | Null): MyOption[T] = ???

def test =
  val g: AnyRef => Boolean = f {
    x => x eq null // ok
  }
  val g2: AnyRef => Boolean = f2 {
    x => x eq null // ok
  }
  val g3: AnyRef => Boolean = f3 {
    x => x eq null // was error
  }
  val g4: AnyRef => Boolean = f4 {
    x => x eq null // was error
  }

  val o1: MyOption[String] = MyOption(null)
  val o2: MyOption[String => Boolean] = MyOption {
    x => x.length > 0
  }
  val o3: MyOption[(String, String) => Boolean] = MyOption {
    (x, y) => x.length > y.length
  }


class Box[T]
val box: Box[Unit] = ???
def ff1[T, U](x: T | U, y: Box[U]): T = ???
def ff2[T, U](x: T & U): T = ???

def test2 =
  val a1: Any => Any = ff1(x => x, box)
  val a2: Any => Any = ff2(x => x)