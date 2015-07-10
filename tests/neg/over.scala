trait T {
  def x = 1
}

class C extends T {

  val x = 2                // error
  override val y = 2       // error

}

class D extends T {

  def x(): String = ""     // error

}


