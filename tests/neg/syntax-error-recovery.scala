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

  def foo3(x: Int) =
    foo2(x
    if x == 0 then println(bar) // error
  println(bar) // error

  def foo4(x: Int) =
    if x < 0) then // error
      1
    else
      2
  println(bam) // error

}
object Test2:
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

  def foo3(x: Int) =
    foo2(x
    if x == 0 then println(bar) // error
  println(bar) // error

  def foo4(x: Int) =
    if x < 0) then // error
      1
    else
      2
  println(bam) // error

  def foo5(x: Int) =
    foo2(foo2(,) // error // error

  println(bam) // error
// error