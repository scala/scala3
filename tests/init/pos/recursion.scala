class A {

  val f: Int => Int = {
    x => f(x)
  }

  f(5)

}
