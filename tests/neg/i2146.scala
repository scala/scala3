class Test {
  def foo[A, B]: given A => given B => Int = { given b: B =>  // error: found Int, required: given A => given B => Int
    42
  }
}
