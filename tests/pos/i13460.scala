import scala.compiletime.*
import scala.deriving.Mirror

class Lazy[A](obj: => A) {
  lazy val value: A = obj
}
object Lazy {
  given [A](using obj: => A ): Lazy[A] = new Lazy(obj)
}

trait MyTypeClass[A] {
  def makeString(a: A): String
}
object MyTypeClass {

  given IntTypeClass: MyTypeClass[Int] with
    def makeString(a: Int): String = a.toString

  inline given derived[A](using m: Mirror.Of[A]): MyTypeClass[A] =
    inline m match
      case p: Mirror.ProductOf[A] => productConverter(p)


  private inline def summonElementTypeClasses[A](m: Mirror.Of[A]): IArray[Object] =
    // this doesn't work
    summonAll[Tuple.Map[m.MirroredElemTypes, [A] =>> Lazy[MyTypeClass[A]]]].toIArray
    // but this does
    // summonAll[Tuple.Map[Tuple.Map[m.MirroredElemTypes, MyTypeClass], Lazy]].toIArray

  private inline def productConverter[A](m: Mirror.ProductOf[A]): MyTypeClass[A] = {
    val elementTypeClasses = summonElementTypeClasses(m)
    new MyTypeClass[A] {
      def makeString(a: A): String = {
        val product = a.asInstanceOf[Product]
        elementTypeClasses
          .view
          .zipWithIndex
          .map((obj, i) => {
            val tc = obj.asInstanceOf[Lazy[MyTypeClass[Any]]].value
            tc.makeString(product.productElement(i))
          })
          .mkString("[", ", ", "]")
      }
    }
  }
}

case class Example(a: Int, b: Int) derives MyTypeClass

object Main {
  def main(args: Array[String]): Unit = {
    println("hello world")
    println(summon[MyTypeClass[Example]].makeString(Example(1,2)))
  }
}