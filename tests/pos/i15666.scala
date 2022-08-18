trait GetInt {
  def value: Int // if we add inline here, the program compiles
}

class Newtype {
  def f: Int = ???

  val g = new GetInt {
    inline def value: Int = f // has to be inline to crash
  }
}