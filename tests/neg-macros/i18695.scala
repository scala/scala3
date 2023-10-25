import scala.annotation.{tailrec, unused}
import scala.deriving.Mirror
import scala.quoted.*

trait TypeLength[A] {
  type Length <: Int
  def length: Length
}
object TypeLength extends TypeLengthLowPriority:
  type Aux[A, Length0 <: Int] = TypeLength[A] {
    type Length = Length0
  }

  transparent inline given fromMirror[A](using m: Mirror.Of[A]): TypeLength[A] =
    ${ macroImpl[A, m.MirroredElemTypes] }

  @tailrec
  private def typesOfTuple(
      using q: Quotes
  )(tpe: q.reflect.TypeRepr, acc: List[q.reflect.TypeRepr]): List[q.reflect.TypeRepr] =
    import q.reflect.*
    val cons = Symbol.classSymbol("scala.*:")
    tpe.widenTermRefByName.dealias match
      case AppliedType(fn, tpes) if defn.isTupleClass(fn.typeSymbol) =>
        tpes.reverse_:::(acc)
      case AppliedType(tp, List(headType, tailType)) if tp.derivesFrom(cons) =>
        typesOfTuple(tailType, headType :: acc)
      case tpe =>
        if tpe.derivesFrom(Symbol.classSymbol("scala.EmptyTuple")) then acc.reverse
        else report.errorAndAbort(s"Unknown type encountered in tuple ${tpe.show}")

  def macroImpl[A: Type, T <: Tuple: scala.quoted.Type](
      using q: scala.quoted.Quotes
  ): scala.quoted.Expr[TypeLength[A]] =
    import q.reflect.*
    val l = typesOfTuple(TypeRepr.of[T], Nil).length
    ConstantType(IntConstant(l)).asType match
      case '[lt] =>
        val le = Expr[Int](l).asExprOf[lt & Int]
        '{
          val r: TypeLength.Aux[A, lt & Int] = new TypeLength[A] {
            type Length = lt & Int
            val length: Length = ${ le }
          }
          r
        }

  transparent inline given fromTuple[T <: Tuple]: TypeLength[T] =
    ${ macroImpl[T, T] }

trait TypeLengthLowPriority:
  self: TypeLength.type =>
  given tupleFromMirrorAndLength[A, T <: Tuple](
      using @unused m: Mirror.Of[A] { type MirroredElemTypes = T },
      length: TypeLength[A]
  ): TypeLength.Aux[T, length.Length] = length.asInstanceOf[TypeLength.Aux[T, length.Length]]

trait HKDSumGeneric[A]
object HKDSumGeneric:
  type NotZero[N <: Int] = N match
    case 0 => false
    case _ => true

  transparent inline given derived[A](using m: Mirror.SumOf[A], typeLength: TypeLength[A])(
      using NotZero[typeLength.Length] =:= true
  ): HKDSumGeneric[A] =
    derivedImpl[A, m.MirroredElemTypes, m.MirroredLabel] // error

  def derivedImpl[A, ElemTypes <: Tuple, Label <: String](
      using m: Mirror.SumOf[A] {
        type MirroredElemTypes = ElemTypes; type MirroredLabel = Label;
      },
      typeLength: TypeLength[ElemTypes],
      nz: NotZero[typeLength.Length] =:= true
  ): HKDSumGeneric[A] = ???