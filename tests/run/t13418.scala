class A { class B }
val a1 = new A
val a2 = new A
val b: Any = new a1.B

class X { class Y[Q] }
val x1 = new X
val x2 = new X
val y: Any = new x1.Y[Int]
type Foo = [Q] =>> x2.Y[Q]
type Bar = [Q] =>> x1.Y[Q]

@main def Test() = {
  b match {
    case _: a2.B => println("wrong")
    case _: a1.B => println("ok")
  }
  y match {
    case _: x2.Y[_] => println("wrong")
    case _: x1.Y[_] => println("ok")
  }
  y match {
    case _: Foo[_] => println("wrong")
    case _: Bar[_] => println("ok")
  }
}
