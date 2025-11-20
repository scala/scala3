//> using options -Wall

class A:
  protected val x: String = ""

class B extends A:
  private val x: Int = 0 // warn // warn unused and shadowing
