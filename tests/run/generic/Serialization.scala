package generic

import java.io.{DataInputStream,DataOutputStream}
import scala.collection.IterableFactory
import scala.collection.mutable.ArrayBuffer
import Shapes.*

object Serialization {

  trait Serializable[T] {
    def write(x: T, out: DataOutputStream): Unit
    def read(in: DataInputStream): T
  }

  implicit val UnitSerializable: Serializable[Unit] =
    new Serializable[Unit] {
      def write(x: Unit, out: DataOutputStream) = ()
      def read(in: DataInputStream) = ()
    }

  implicit def SingleSerializable[T](implicit
      ev1: Singleton[T]
  ): Serializable[T] = new Serializable[T] {
    def write(x: T, out: DataOutputStream) = ()
    def read(in: DataInputStream) = ev1.value
  }

  implicit def EnumValueSerializable[T]: Serializable[EnumValue[T]] =
    new Serializable[EnumValue[T]] {
      def write(x: EnumValue[T], out: DataOutputStream) = out.writeShort(x.tag)
      def read(in: DataInputStream) = EnumValue(in.readShort())
    }

  implicit val BooleanSerializable: Serializable[Boolean] =
    new Serializable[Boolean] {
      def write(x: Boolean, out: DataOutputStream) = out.writeBoolean(x)
      def read(in: DataInputStream) = in.readBoolean()
    }

  implicit val IntSerializable: Serializable[Int] =
    new Serializable[Int] {
      def write(x: Int, out: DataOutputStream) = out.writeInt(x)
      def read(in: DataInputStream) = in.readInt()
    }

  implicit val StringSerializable: Serializable[String] =
    new Serializable[String] {
      def write(x: String, out: DataOutputStream) = out.writeUTF(x)
      def read(in: DataInputStream) = in.readUTF()
    }

  def RecSerializable[T, U](implicit
      ev1: T unfolds U,
      ev2: Serializable[U]
    ): Serializable[T]  =
    new Serializable[T] {
      def write(x: T, out: DataOutputStream) = ev2.write(ev1.toShape(x), out)
      def read(in: DataInputStream) = ev1.fromShape(ev2.read(in))
    }

  implicit def ShapedSerializable[T, U](implicit
      ev1: T shaped U,
      ev2: Serializable[U]
    ): Serializable[T]  =
    new Serializable[T] {
      def write(x: T, out: DataOutputStream) = ev2.write(ev1.toShape(x), out)
      def read(in: DataInputStream) = ev1.fromShape(ev2.read(in))
    }

  implicit def SumSerializable[T, U](implicit
    // parameters need to be call by name, or we get a recursive lazy val definition in materialized code
    ev1: => Serializable[T],
    ev2: => Serializable[U]
  ): Serializable[Sum[T, U]] =
    new Serializable[Sum[T, U]] {
      def write(x: Sum[T, U], out: DataOutputStream): Unit = x match {
        case Fst(y) => out.writeBoolean(false); ev1.write(y, out)
        case Snd(y) => out.writeBoolean(true); ev2.write(y, out)
      }
      def read(in: DataInputStream) = in.readBoolean() match {
        case false => Fst(ev1.read(in))
        case true => Snd(ev2.read(in))
      }
    }

  implicit def ProdSerializable[T, U](implicit
    ev1: Serializable[T],
    ev2: Serializable[U]
  ): Serializable[Prod[T, U]] =
    new Serializable[Prod[T, U]] {
      def write(x: Prod[T, U], out: DataOutputStream): Unit = {
        ev1.write(x.fst, out)
        ev2.write(x.snd, out)
      }
      def read(in: DataInputStream) = {
        Prod(ev1.read(in), ev2.read(in))
      }
    }

  implicit def IterableSerializable[I[X] <: Iterable[X], Elem](implicit
    ev1: IterableFactory[I],
    ev2: Serializable[Elem]
  ): Serializable[I[Elem]] =
    new Serializable[I[Elem]] {
      def write(xs: I[Elem], out: DataOutputStream) = {
        out.writeInt(xs.size)
        xs.foreach(ev2.write(_, out))
      }
      def read(in: DataInputStream) = {
        val bldr = ev1.newBuilder[Elem]
        for (i <- 0 until in.readInt()) bldr += ev2.read(in)
        bldr.result()
      }
    }
}
