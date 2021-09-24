object Test:
  var cnt = 0

  trait Foo:
    lazy val foo1 = {cnt+=1; cnt}

    @scala.annotation.targetName("fooTwo")
    lazy val foo2 = {cnt+=1; cnt}

  object Bar extends Foo

  def main(sa: Array[String]): Unit =

    println(Bar.foo1)  // Prints 1
    println(Bar.foo1)  // Prints 1
    println(Bar.foo1)  // Prints 1
    println(Bar.foo1)  // Prints 1

    println(Bar.foo2)  // Prints 2
    println(Bar.foo2)  // Prints 3  EXPECTED 2
    println(Bar.foo2)  // Prints 4  EXPECTED 2
    println(Bar.foo2)  // Prints 5  EXPECTED 2