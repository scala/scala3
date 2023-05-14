opaque type Bytes = Array[Byte]
object Bytes:
  extension (self: Bytes)
    def size: Int = (self: Array[Byte]).size  // error

//

object Module1:
  opaque type State[S, +A] = S => (S, A)
  object State:
    extension [S, A](self: State[S, A])
      def map[B](f: A => B): State[S, B] =
        s => { val (s2, a) = self(s); (s2, f(a)) }
object Module2:
  import Module1.State
  trait RNG
  opaque type Gen[+A] = State[RNG, A]
  object Gen:
    extension [A](self: Gen[A])
      def map[B](f: A => B): Gen[B] =
        self.map(f)  // error

//

class Sym(val owner: Sym)

extension (sym: Sym)
  def isSomething: Boolean = false
  def isFoo: Boolean       = sym.isSomething && sym.owner.isFoo // was: Infinite loop in function body
  def isBar: Boolean       = sym.isSomething || sym.owner.isBar // was: Infinite loop in function body
