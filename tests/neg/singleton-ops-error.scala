import scala.compiletime.ops.int.{ToString, >=}
import scala.compiletime.ops.string.{+, Error}

object Test {
  type Require[Cond <: Boolean, Msg <: String] = Cond match {
    case true => Any
    case false => Error[Msg]
  }

  opaque type Positive = Int

  object Positive {
    type RequirePositive[T <: Int] = Require[T >= 0, "The provided value (" + ToString[T] + ") isn't positive"]
    def apply[T <: Int](value: T)(given RequirePositive[T]): Positive = value
  }

  val t0: Positive = Positive[1](1)
  val t1: Positive = Positive[-1](-1) // error
  val t2 = Positive[1](1)
  val t3 = Positive[-1](-1) // error

  val err: Error["My error message"] = ??? // error
}
