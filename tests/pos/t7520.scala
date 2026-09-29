class A {
  val x: Singleton & this.type = this
  val y: this.type = x
}

class B {
  val x = ""
  val xs: x.type & Singleton = x
  val y: x.type = xs
}
