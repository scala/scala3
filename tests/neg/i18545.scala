package iolib:
  case class IO[A](value: A)

  sealed trait IOLocal[A]

  object IOLocal:
    def apply[A](default: A): IO[IOLocal[A]] = IO(new IOLocalImpl(default))

    private[IOLocal] final class IOLocalImpl[A](default: A) extends IOLocal[A]

package tests:
  import iolib.IOLocal
  def test: IOLocal.IOLocalImpl[Int] = // error
    IOLocal.IOLocalImpl.apply(42) // error
  def test2 = IOLocal.IOLocalImpl(42) // error
