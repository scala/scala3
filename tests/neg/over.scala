trait T {
  def x = 1
}

class C extends T {

  val x = 2
  override val y = 2

}

class D extends T {

  def x(): String = ""

}


