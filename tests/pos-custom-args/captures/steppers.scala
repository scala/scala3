
trait Stepper[+A]

object Stepper:
  trait EfficientSplit

sealed trait StepperShape[-T, S <: Stepper[_]^] extends Pure

trait IterableOnce[+A] extends Any:
  def stepper[S <: Stepper[_]^{this}](implicit shape: StepperShape[A, S]): S = ???

sealed abstract class ArraySeq[T] extends IterableOnce[T], Pure:
  def array: Array[_]

  def sorted[B >: T](implicit ord: Ordering[B]): ArraySeq[T] =
    val arr = array.asInstanceOf[Array[T]].sorted(ord.asInstanceOf[Ordering[Any]]).asInstanceOf[Array[T]]
    ArraySeq.make(arr).asInstanceOf[ArraySeq[T]]

object ArraySeq:

  def make[T](x: Array[T]): ArraySeq[T] = ???

  final class ofRef[T <: AnyRef](val array: Array[T]) extends ArraySeq[T], Pure:
    override def stepper[S <: Stepper[_]](implicit shape: StepperShape[T, S]): S & Stepper.EfficientSplit = ???

