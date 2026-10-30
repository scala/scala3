class Test {
  def f = {
    do {
      println(1)
    } while {
      println(2)
      false
    }
    do println(3) while (false)
  }
}
