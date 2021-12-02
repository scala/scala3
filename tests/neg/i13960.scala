class A() extends Product {
  override def canEqual(that: Any) = true
  override def productArity = 0
  override def productElement(n: Int) = null
}

object A {
  def unapply(a: A): A = a
}

object Main {
  (new A) match {
    case A() => // error
  }
}
