object Test {
  var b = true
  def foo erased (a: Boolean): Boolean = {
    if (a) // error
      true
    else
      false

    if ({
      println()
      a // error
    })
      true
    else
      false

    if (b)
      a // error
    else
      a // error
  }
}
