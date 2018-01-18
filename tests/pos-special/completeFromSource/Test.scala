package completeFromSource

class Test extends nested.A(22) {

  val y: Int = this.x

  val a = nested.A(33)

  println(a.x)

}


