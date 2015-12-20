// Invalid because lambdas can no longer be tenmplate statements.
object Test {
  trait Suite { def bar() = () }

  () => {
    trait FunkySuite extends Suite { override def bar() = () }
    class MySuite extends FunkySuite { }
  }
}
