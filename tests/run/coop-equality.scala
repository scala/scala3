object Test extends App {

      class ANY {
        def ===(that: ANY) = this eq that
      }

      case class A(x: String) extends ANY {
        def ===(that: A) = this.x == that.x
        def ===(that: B) = this.x == that.x
      }

      case class B(x: String) extends ANY {
        def ===(that: A) = this.x == that.x
        def ===(that: B) = this.x == that.x
      }

      val a = A("")
      val b = B("")

  assert(a === b)
  assert(!((a: ANY) === (b: ANY)))
}