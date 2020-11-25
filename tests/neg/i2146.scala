class Test {
  def foo[A, B]: A ?=> B ?=> Int = { (b: B) ?=>  // error: found Int, required: A ?=> B ?=> Int
    42
  }
}
