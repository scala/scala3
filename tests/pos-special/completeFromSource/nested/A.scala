package completeFromSource.nested

class A(y: Int) {

  val x: Int = y

}

object A {

  def apply(x: Int) = new A(22)

}
