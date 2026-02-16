trait Dsl:

    sealed trait Nat
    case object Zero extends Nat
    case class Succ[N <: Nat](n: N) extends Nat

    type Stable[+l <: Nat, +b <: Nat, +A]
    type Now[+l <: Nat, +b <: Nat, +A]
    type Box[+A]
    def stable[l <: Nat, b <: Nat, A](e: Stable[l, b, A]): Now[l, b, Box[A]]

    def program[A](prog: Now[Zero.type, Zero.type, A]): Now[Zero.type, Zero.type, A]

    //val conforms: Zero.type <:< Nat = summon
    // ^ need to uncomment this line to compile with captureChecking enabled

    def test: Any =
        program[Box[Int]]:
            val v  : Stable[Zero.type, Zero.type, Int] = ???
            stable[Zero.type, Zero.type, Int](v)
//          ^
// Type argument Dsl.this.Zero.type does not conform to upper bound Dsl.this.Nat