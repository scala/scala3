def test(xs: List[() => Unit]) =
  xs.head // error

  def foo =
    xs.head // error, ok under deferredReaches
  def bar() =
    xs.head // error, ok under deferredReaches

  class Foo:
    println(xs.head) // error, but could be OK

  foo // error
  bar() // error
  Foo() // error

def test2(xs: List[() => Unit]) =
  def foo = xs.head // error, ok under deferredReaches
  ()

def test3(xs: List[() => Unit]): () ->{xs*} Unit = () =>
  println(xs.head)  // error, ok under deferredReaches

  def test4(xs: List[() => Unit]) = () => xs.head // error, ok under deferredReaches

  def test5(xs: List[() => Unit]) = new:
    println(xs.head) // error, ok under deferredReaches

  def test6(xs: List[() => Unit]) =
    val x= new { println(xs.head) } // error
    x
