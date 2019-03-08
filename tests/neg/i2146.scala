object Test {
  case class A()
  case class B()

  def foo[A, B]: given A => given B => Int = { given b: B =>
    42 // error: found Int, required: given A => given B => Int
  }
}
