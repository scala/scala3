package test

class Sett[A] {

  def incl(elem: A): Sett[A] = ???

  inline final def + (elem: A): Sett[A] = incl(elem)
}

object Sett {
  def apply[A](elems: A*): Sett[A] = ???
}

class Test {
  Sett(1) + 1
}
