class Test {
  def foo[A, B]: A ?=> B ?=> Int = { (using b: B) =>  // error: found Int, required: A ?=> B ?=> Int
    42
  }
}
