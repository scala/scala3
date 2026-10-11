trait A:

  def f(): A^
  def g(x: A^): A^

class B extends A, Product:
  consume def f(): A^ = this    // error
  def g(consume x: A^): A^ = x  // error

  consume def productElement(n: Int): Any = ??? // error
  def canEqual(that: Any): Boolean = ???
  def productArity: Int = ???
