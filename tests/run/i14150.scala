import scala.deriving.Mirror
import scala.util.NotGiven
import scala.compiletime.constValue

trait GetConstValue[T] {
    type Out
    def get : Out
}

object GetConstValue {
    type Aux[T, O] = GetConstValue[T] { type Out = O }

    inline given value[T <: Singleton](
        using
        ev : NotGiven[T <:< Tuple],
    ) : GetConstValue.Aux[T, T] = {
        val out = constValue[T]

        new GetConstValue[T] {
            type Out = T
            def get : Out = out
        }
    }

    given empty : GetConstValue[EmptyTuple] with {
        type Out = EmptyTuple
        def get : Out = EmptyTuple
    }

    given nonEmpty[H, HRes, Tail <: Tuple, TRes <: Tuple](
        using
        head : GetConstValue.Aux[H, HRes],
        tail : GetConstValue.Aux[Tail, TRes],
    ) : GetConstValue[H *: Tail] with {
        type Out = HRes *: TRes

        def get : Out = head.get *: tail.get
    }
}

trait MirrorNamesDeriver[T] {
    type Derived <: Tuple
    def derive : Derived
}

object MirrorNamesDeriver {
    given mirDeriver[T, ElemLabels <: NonEmptyTuple](
        using
        mir: Mirror.SumOf[T] { type MirroredElemLabels = ElemLabels },
        ev : GetConstValue.Aux[ElemLabels, ElemLabels],
    ): MirrorNamesDeriver[T] with {
        type Derived = ElemLabels

        def derive: ElemLabels = ev.get
    }

    def derive[T](using d : MirrorNamesDeriver[T]) : d.Derived = d.derive
}

sealed trait SuperT
final case class SubT1(int: Int) extends SuperT
final case class SubT2(str: String, dbl : Double, bool : Boolean) extends SuperT

@main def Test =

  // Works when type parameters are set explicitly
  val successfulLabels = MirrorNamesDeriver.mirDeriver[SuperT, ("SubT1", "SubT2")].derive
  println(successfulLabels)
  assert(successfulLabels == ("SubT1", "SubT2"))

  // Fails when type parameters are inferred
  val failedLabels = MirrorNamesDeriver.derive[SuperT]
  println(successfulLabels)
  assert(failedLabels == ("SubT1", "SubT2"))
