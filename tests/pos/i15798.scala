import compiletime.ops.*

class Inlined[T](val value : T)
object Inlined:
  trait TC[UB, R]:
    type Out <: UB
    def apply(arg: R): Inlined[Out]
  object TC:
    transparent inline given fromVal[UB, R <: UB]: TC[UB, R] = new TC[UB, R]:
      type Out = R
      def apply(arg: R): Inlined[R] = forced[R](arg)
    transparent inline given fromInline[UB, R <: UB, I <: Inlined[R]]: TC[UB, I] =
      new TC[UB, I]:
        type Out = R
        def apply(arg: I): Inlined[R] = arg

  protected inline def forced[T](_value: Any): Inlined[T] = Inlined[T](_value.asInstanceOf[T])

  def add[T <: Int, R](lhs: Inlined[T], rhs: R)(using tc: TC[Int, R]) =
      forced[int.+[T, tc.Out]](lhs.value + tc(rhs).value)
  def get[T <: Int](inlined: Inlined[T]) : T = inlined.value

object Test:
  inline def check[UB <: Int](ub: Inlined[UB]): Unit =
    val y = Inlined.get(Inlined.add(ub, 1))
