class TC
object A {
  implied tc for TC
  def foo given TC = ()
}
object B {
  import A._
  foo            // error: no implicit argument was found
  foo given tc   // error: not found: tc
  foo given A.tc // ok
}
object C {
  import A._
  import implied A.tc
  foo            // ok
  foo given tc   // ok
}
object D {
  import A.foo
  import implied A._
  foo            // ok
  foo given tc   // ok
}
object E {
  import A._
  import implied A._
  foo            // ok
  foo given tc   // ok
}
