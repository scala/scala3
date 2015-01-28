abstract class A { type T }

abstract class B { val xz: Any }

abstract class Test {
  var yy: A with B { type T; val xz: T } = null;
  var xx: A with B { type T; val xz: T } = null;
  xx = yy;
}

abstract class Test2 {
  var yy: A with B { type T; val xz: T } = null;
  val xx: A with B { type T; val xz: T } = yy
}
