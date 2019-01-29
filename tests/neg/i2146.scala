object Test {
  case class A()
  case class B()

  def foo[A, B]: A |=> B |=> Int = { b: B |=>
    42 // error: found Int, required: A |=> B |=> Int
  }
}
