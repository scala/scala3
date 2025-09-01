import scala.deriving.*
import scala.compiletime.{erasedValue, summonInline}

case class Simple(a: String, b: Boolean) derives Printable
case class SimpleT(a: (String, Boolean)) derives Printable

@main def Test: Unit = {

  summon[Printable[Simple]].print // Prints STRING BOOLEAN as expected

  summon[Printable[SimpleT]].print // java.lang.ClassCastException: SimpleT$$anon$1 cannot be cast to scala.deriving.Mirror$Product

}

trait Printable[T]:
   def print: Unit

object Printable:

   given Printable[String]:
      def print: Unit = println("STRING")

   given Printable[Boolean]:
      def print: Unit = println("BOOLEAN")

   def printProduct[T](p: Mirror.ProductOf[T], elems: => List[Printable[_]]): Printable[T] =
      new Printable[T]:
         def print: Unit =
            elems.foreach(_.print)

   inline given derived: [T] => (m: Mirror.Of[T]) => Printable[T] =
      val elemInstances = summonAllPrintable[m.MirroredElemTypes]
      inline m match
         case p: Mirror.ProductOf[T] => printProduct(p, elemInstances)

end Printable

inline def summonAllPrintable[T <: Tuple]: List[Printable[_]] =
   inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (t *: ts) => summonInline[Printable[t]] :: summonAllPrintable[ts]
