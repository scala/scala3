object Foo {
  val x = new Object

  class A(var y: x.type)

  val a = new A(x)

  val y: a.y.type = x
// 1 |val y: a.y.type = x
//   |         ^
//   |         Object(x)(a.y) is not a legal path
//   |         since it refers to nonfinal variable y
}
