class A {
  this.synchronized { 5 }
  "hello" eq this
  "hello" ne this
  this.getClass

  lazy val a: Int = List(3, 5).size
  a
}