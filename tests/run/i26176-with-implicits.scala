trait Sp[T]

class C[T](val xs: List[T])(using ev: Sp[T]):
  inline def m: C[T] = new C[T](xs) {}

object Test:
  given Sp[Int] = new Sp[Int] {}
  def main(args: Array[String]): Unit =
    val c = new C[Int](List(1, 2, 3))
    c.m
