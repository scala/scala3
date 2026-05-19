import caps.unsafe.untrackedCaptures

trait Stepper[+A]

object Stepper:
  trait EfficientSplit

sealed trait StepperShape[-T, S <: Stepper[?]^] extends caps.Pure

trait IterableOnce[+A] extends Any:
  def stepper[S <: Stepper[?]^{this}](implicit shape: StepperShape[A, S]): S = ???

sealed abstract class ArraySeq[T] extends IterableOnce[T], caps.Pure:
  val array: Array[?]

  def sorted[B >: T](implicit ord: Ordering[B]): ArraySeq[T] =
    val arr = array.asInstanceOf[Array[T]].sorted(using ord.asInstanceOf[Ordering[Any]]).asInstanceOf[Array[T]]
    ArraySeq.make(arr).asInstanceOf[ArraySeq[T]]

object ArraySeq:

  def make[T](x: Array[T]): ArraySeq[T] = ???

  final class ofRef[T <: AnyRef](@untrackedCaptures val array: Array[T]) extends ArraySeq[T], caps.Pure:
    override def stepper[S <: Stepper[?]](implicit shape: StepperShape[T, S]): S & Stepper.EfficientSplit = ???

