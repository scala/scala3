object test {

  def foo(x: Int) = x + 1 ensuring { y => y >= 0 }

}
