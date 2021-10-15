enum Example[-I, +O]:
  case Pure[A](value: A) extends Example[A, A]
  case Zip[I1, O1, I2, O2, RI, RO](a: Example[I1, O1], b: Example[I2, O2]) extends Example[RI, RO]

object Example:
  def pure[A](value: A): Example[A, A] = Example.Pure(value)

extension[I, O] (self: Example[I, O])
  def zip[I2, O2, RI, RO](other: Example[I2, O2])(using Unzippable.In[I, I2, RI], Zippable.Out[O, O2, RO]): Example[RI, RO] =
    Example.Zip(self, other)


trait Unzippable[A, B]:
  type In
  def unzip(in: In): (A, B)

object Unzippable extends UnzippableLowPriority1:
  type In[A, B, C] = Unzippable[A, B] { type In = C }

  implicit def UnzippableLeftIdentity[A]: Unzippable.In[Unit, A, A] =
    new Unzippable[Unit, A] {
      type In = A
      def unzip(in: A): (Unit, A) =
        ((), in)
    }

trait UnzippableLowPriority1 {

  implicit def Unzippable2[A, B]: Unzippable.In[A, B, (A, B)] =
    new Unzippable[A, B] {
      type In = (A, B)
      def unzip(in: (A, B)): (A, B) =
        (in._1, in._2)
    }
}

trait Zippable[-A, -B]:
  type Out
  def zip(left: A, right: B): Out

object Zippable:
  type Out[-A, -B, C] = Zippable[A, B] { type Out = C }

  implicit def Zippable2[A, B]: Zippable.Out[A, B, (A, B)] =
    new Zippable[A, B] {
      type Out = (A, B)
      def zip(left: A, right: B): Out = (left, right)
    }

def assertSameType[T](e: Example[T, T]): Example[T, T] = e

object Main:
  import Example._

  def main(args: Array[String]): Unit =
    val x = pure(1)
    val y = pure(2)

    val xy1: Example[(Int, Int), (Int, Int)] = x zip y
    // val xy2: Example[(Int, Int), (Int, Int)] = pure(1) zip pure(2)
    val xy2 = pure(1) zip pure(2)

    val _ = assertSameType(xy1)
    val _ = assertSameType(xy2)
