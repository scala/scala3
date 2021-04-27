// This test source should be invariant under the following 4 compilation steps with options
//  -rewrite -new-syntax
//  -rewrite -indent
//  -rewrite -no-indent
//  -rewrite -old-syntax
object test {

  for {
    x <- List(1, 2, 3)
  }
    println(x)

  for  (x <- List(1, 2, 3))  yield x

  for {
    x <- List(1, 2, 3)
    if x == 0
  }
  println(x)

  def foo = {
    println("hi")
    println("ho")
    // this comment goes inside braces
  }
  // this comment follows the brace
  // this comment as well
  object o {
  }

  def loop[T]()(x: T): T = x

  def g() = /*>*/ loop() /*<*/ {
    println()
    1
  }

  def bar() = { /* */
  }
}
