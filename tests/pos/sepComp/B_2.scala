package sepComp

class B extends A(22) {

  val y: Int = this.x

  val a = A(33)

  println(a.x)

}


