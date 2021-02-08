trait M:
  type X
  object X:
    def foo(): X = ???

transparent inline def m(using m: M): m.type = m

def doSomething(body: M ?=> Unit) = body(using new M{})


def Test1 =
  given M = new M{}
  import m.*
  val x: X = X.foo()
  println(x)

def Test2 =

  doSomething {
    val x: m.X = m.X.foo()
    println(x)
  }
  // or with an import
  doSomething {
    import m._ // Concise and clear import of the same stable path `m`
    val x: X = X.foo()
    println(x)
  }
  // without this feature we would need an extra line in each call site
  doSomething {
    // not ideal
    val myM = m // or summon[M]
    import myM.*
    val x: X = X.foo()
    println(x)
  }
