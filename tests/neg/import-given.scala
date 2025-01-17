class TC
object A {
  given tc: TC()
  def foo(using TC) = ()
}
object B {
  import A.*
  foo             // error: no implicit argument was found
  foo(using tc)   // error: not found: tc
  foo(using A.tc) // ok
}
object C {
  import A.*
  import A.tc
  foo            // ok
  foo(using tc)  // ok
}
object D {
  import A.{foo, given}
  foo            // ok
  foo(using tc)  // ok
}
object E {
  import A.{_, given}
  foo            // ok
  foo(using tc)  // ok
}
object F:
  import A.{given ?}  // error: unbound wildcard type

