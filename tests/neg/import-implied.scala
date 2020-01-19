class TC
object A {
  given tc as TC
  def foo with TC = ()
}
object B {
  import A._
  foo             // error: no implicit argument was found
  foo.with(tc)   // error: not found: tc
  foo.with(A.tc) // ok
}
object C {
  import A._
  import A.tc
  foo            // ok
  foo.with(tc)  // ok
}
object D {
  import A.{foo, given _}
  foo            // ok
  foo.with(tc)  // ok
}
object E {
  import A.{_, given _}
  foo            // ok
  foo.with(tc)  // ok
}
