import scala.deriving.Mirror
import scala.quoted.{Expr, Quotes, Type}

trait macroTest[A] {
  type ElemTop <: A
  type Index[_]

  case class IdxWrapper[X](idx: Index[X])

  def indexOf[X <: ElemTop: Type](x: Expr[X]): Expr[Index[X]]

  def indexOfA(a: Expr[A]): Expr[IdxWrapper[_ <: ElemTop]]
}
object macroTest {

  def derivedImpl[A: Type, ElemTypes <: Tuple: Type, Label <: String: Type, Labels <: Tuple: Type](
      using m: Expr[
        Mirror.SumOf[A] {
          type MirroredElemTypes  = ElemTypes
          type MirroredLabel      = Label
          type MirroredElemLabels = Labels
        }
      ],
      q: Quotes,
  ): macroTest[A] = new macroTest[A]:
    override type Index[_] = Int

    override def indexOf[X <: ElemTop: Type](x: Expr[X]): Expr[Index[X]] = '{ $m.ordinal($x) }

    override def indexOfA(a: Expr[A]): Expr[IdxWrapper[_ <: ElemTop]] =
      given Type[IdxWrapper] = Type.of[IdxWrapper] // error
      given Type[ElemTop] = Type.of[ElemTop] // error
      '{ new IdxWrapper(${ indexOf(a.asInstanceOf[Expr[ElemTop]]) }) } // error // error
}
