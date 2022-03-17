object Test {
  def foo(x: Int) = {
      if (x < 0 then // error
        1
      else
        2
  }
  println(bar) // error

  def foo2(x: Int) =
      if (x < 0 then // error
        1
      else
        2
  println(baz) // error

}


