def foo(f: => () => Unit): Unit = ???
def boo(f: [A] => () => Unit): Unit = ???

object test:
  foo { () =>  // okay
    println(1)
    println(2)
  }

  boo { [A] => () => // error
    println(1)
    println(2)
  }

  boo { [A] => () => { // okay
    println(1)
    println(2)
  }}