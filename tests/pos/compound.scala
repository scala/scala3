abstract class A { type T }

abstract class B { val xz: Any }

abstract class Test {
  var yy: A & B { type T; val xz: T } = ???;
  var xx: A & B { type T; val xz: T } = ???;
  xx = yy;
}

abstract class Test2 {
  var yy: A & B { type T; val xz: T } = ???;
  val xx: A & B { type T; val xz: T } = yy
}
