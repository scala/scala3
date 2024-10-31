def test(xs: List[() => Unit]) =
  xs.head // error

  def foo =
    xs.head // ok
  def bar() =
    xs.head // ok

  class Foo:
    println(xs.head) // error, but could be OK

  foo // error
  bar() // error
  Foo() // OK, but could be error

def test2(xs: List[() => Unit]) =
  def foo = xs.head // ok
  ()

def test3(xs: List[() => Unit]): () ->{xs*} Unit = () =>
  println(xs.head)  // ok

  def test4(xs: List[() => Unit]) = () => xs.head // ok

  def test5(xs: List[() => Unit]) = new:
    println(xs.head) // ok

  def test6(xs: List[() => Unit]) =
    val x= new { println(xs.head) } // error
    x
