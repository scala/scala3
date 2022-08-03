import scala.collection.mutable
import scala.annotation.tailrec

// derived from tests/run/typeclass-derivation3.scala, but with hierarchical ADT as an inner class.

class Datatypes {
  import typeclasses.*

  sealed trait JsonSelect extends Selectable { self: JsonObject =>
    def selectDynamic(name: String): Json[Any] =
      fields.collectFirst({
        case (`name`, value) => value
      }).getOrElse(JsonNull)
  }

  sealed trait JsonObjFactory[Ref]:
    type T = JsonObject & Ref
    def apply(fields: (String, Json[Any])*): T = new JsonObject(fields.toList).asInstanceOf[T]

  sealed abstract class Json[+A]
  sealed abstract class JsonScalar[A] extends Json[A]
  sealed abstract class JsonCompound[A] extends Json[A]
  case class JsonObject(fields: List[(String, Json[Any])]) extends JsonCompound[Any] with JsonSelect
  case class JsonArray[B <: Json[Any]](items: List[B]) extends JsonCompound[List[B]]:
    export items.apply
  case class JsonString(str: String) extends JsonScalar[String]
  case class JsonNumber(num: Double) extends JsonScalar[Double]
  sealed abstract class JsonBoolean extends JsonScalar[Boolean]
  case object JsonTrue extends JsonBoolean
  case object JsonFalse extends JsonBoolean
  case object JsonNull extends JsonScalar[Null]

  def json[B <: Json[Any]](items: B*): JsonArray[B] = JsonArray(items.toList)

  object Json:
    def apply[Ref] = new JsonObjFactory[Ref] {}

  sealed abstract class Lst[+A]  // force anonymous mirror to be generated for Lst
  case class Cs[+A](head: A, tail: Lst[A]) extends Lst[A]
  case object Cs // force anonymous mirror to be generated for Cs
  case object Nl extends Lst[Nothing]

}

object typeclasses {

  // Another type class
  trait Pickler[T] {
    def pickle(buf: mutable.ListBuffer[Int], x: T): Unit
    def unpickle(buf: mutable.ListBuffer[Int]): T
  }

  object Pickler {
    import scala.compiletime.{erasedValue, constValue, summonFrom}
    import compiletime.*
    import deriving.*

    def nextInt(buf: mutable.ListBuffer[Int]): Int = try buf.head finally buf.dropInPlace(1)

    inline def tryPickle[T](buf: mutable.ListBuffer[Int], x: T): Unit = summonFrom {
      case pkl: Pickler[T] => pkl.pickle(buf, x)
    }

    inline def pickleElems[Elems <: Tuple](n: Int)(buf: mutable.ListBuffer[Int], x: Product): Unit =
      inline erasedValue[Elems] match {
        case _: (elem *: elems1) =>
          tryPickle[elem](buf, x.productElement(n).asInstanceOf[elem])
          pickleElems[elems1](n + 1)(buf, x)
        case _: EmptyTuple =>
      }

    transparent inline def pickleCases[Alts <: Tuple](n: Int)(buf: mutable.ListBuffer[Int], x: Any, ord: Int): Unit =
      inline erasedValue[Alts] match {
        case _: (alt *: alts1) =>
          if (ord == n)
            summonFrom {
              case m: Mirror.ProductOf[`alt`] => pickleElems[m.MirroredElemTypes](0)(buf, x.asInstanceOf[Product])
              case m: Mirror.SumOf[`alt`] =>
                val ord1 = m.ordinal(x.asInstanceOf[alt])
                buf += ord1
                pickleCases[m.MirroredElemTypes](0)(buf, x, ord1)
            }
          else pickleCases[alts1](n + 1)(buf, x, ord)
        case _: EmptyTuple =>
      }

    inline def tryUnpickle[T](buf: mutable.ListBuffer[Int]): T = summonFrom {
      case pkl: Pickler[T] => pkl.unpickle(buf)
    }

    inline def unpickleElems[Elems <: Tuple](n: Int)(buf: mutable.ListBuffer[Int], elems: Array[Any]): Unit =
      inline erasedValue[Elems] match {
        case _: (elem *: elems1) =>
          elems(n) = tryUnpickle[elem](buf)
          unpickleElems[elems1](n + 1)(buf, elems)
        case _: EmptyTuple =>
      }

    transparent inline def unpickleCase[T, Elems <: Tuple](buf: mutable.ListBuffer[Int], m: Mirror.ProductOf[T]): T = {
      inline val size = constValue[Tuple.Size[Elems]]
      inline if (size == 0)
        m.fromProduct(EmptyTuple)
      else {
        val elems = new Array[Any](size)
        unpickleElems[Elems](0)(buf, elems)
        m.fromProduct(new Product {
          def canEqual(that: Any): Boolean = true
          def productArity: Int = size
          def productElement(idx: Int): Any = elems(idx)
        })
      }
    }

    transparent inline def unpickleCases[T, Alts <: Tuple](n: Int)(buf: mutable.ListBuffer[Int], ord: Int): T =
      inline erasedValue[Alts] match {
        case _: (alt *: alts1) =>
          if (ord == n)
            summonFrom {
              case m: Mirror.ProductOf[`alt` & T] =>
                unpickleCase[`alt` & T, m.MirroredElemTypes](buf, m)
              case m: Mirror.SumOf[`alt` & T] =>
                val ord1 = nextInt(buf)
                unpickleCases[`alt` & T, m.MirroredElemTypes](0)(buf, ord1)
            }
          else unpickleCases[T, alts1](n + 1)(buf, ord)
        case _: EmptyTuple =>
          throw new IndexOutOfBoundsException(s"unexpected ordinal number: $ord")
      }

    transparent inline def derived[T](implicit ev: Mirror.Of[T]): Pickler[T] = new {
      def pickle(buf: mutable.ListBuffer[Int], x: T): Unit =
        inline ev match {
          case m: Mirror.SumOf[T] =>
            val ord = m.ordinal(x)
            buf += ord
            pickleCases[m.MirroredElemTypes](0)(buf, x, ord)
          case m: Mirror.ProductOf[T] =>
            pickleElems[m.MirroredElemTypes](0)(buf, x.asInstanceOf[Product])
        }
      def unpickle(buf: mutable.ListBuffer[Int]): T =
        inline ev match {
          case m: Mirror.SumOf[T] =>
            val ord = nextInt(buf)
            unpickleCases[T, m.MirroredElemTypes](0)(buf, ord)
          case m: Mirror.ProductOf[T] =>
            unpickleCase[T, m.MirroredElemTypes](buf, m)
        }
    }

    implicit def listPickler[T: Pickler]: Pickler[List[T]] = Pickler.derived

    implicit def tuple2pickler[T: Pickler, U: Pickler]: Pickler[(T, U)] = Pickler.derived

    implicit object IntPickler extends Pickler[Int] {
      def pickle(buf: mutable.ListBuffer[Int], x: Int): Unit = buf += x
      def unpickle(buf: mutable.ListBuffer[Int]): Int = nextInt(buf)
    }

    implicit object NullPickler extends Pickler[Null] {
      def pickle(buf: mutable.ListBuffer[Int], x: Null): Unit = buf += -1
      def unpickle(buf: mutable.ListBuffer[Int]): Null = { val _ = nextInt(buf).ensuring(_ == -1); null }
    }

    implicit object DoublePickler extends Pickler[Double] {
      def pickle(buf: mutable.ListBuffer[Int], x: Double): Unit =
        val asLong = java.lang.Double.doubleToLongBits(x)
        buf += ((asLong >> 32) & 0xFFFFFFFFL).toInt
        buf += (asLong & 0xFFFFFFFFL).toInt
      def unpickle(buf: mutable.ListBuffer[Int]): Double =
        val high = nextInt(buf)
        val low = nextInt(buf)
        java.lang.Double.longBitsToDouble(((high.toLong << 32) | low.toLong) & 0xFFFFFFFFFFFFFFFFL)
    }

    implicit object StringPickler extends Pickler[String] {
      def pickle(buf: mutable.ListBuffer[Int], x: String): Unit = {
        val compressed = x.getBytes.sliding(4, 4).map(_.foldLeft(0)((a, b) => (a << 8) | b)).toArray
        buf += compressed.length
        buf ++= compressed
      }
      def unpickle(buf: mutable.ListBuffer[Int]): String = {
        val len = nextInt(buf)
        val arr = new Array[Int](len)
        buf.copyToArray(arr)
        buf.dropInPlace(len)
        val bbuf = new mutable.ArrayBuffer[Byte]
        val temp = new Array[Byte](4)
        def intToBytes(x: Int, temp: Array[Byte]): Unit = {
          var i = 0
          var z = x
          while z != 0 do // fill temp with bytes until no more in `z`
            temp(i) = (z & 0xFF).toByte
            i += 1
            z = z >> 8
          i -= 1 // set i to last filled index
          while i >= 0 do
            bbuf += temp(i)
            i -= 1
        }
        arr.foreach(intToBytes(_, temp))
        new String(bbuf.toArray)
      }
    }
  }

}
import typeclasses.*
// Tests
@main def Test = {
  val datatypes = new Datatypes()
  import datatypes.*

  type OfLst = deriving.Mirror {
    type MirroredType[X] = Lst[X];
    type MirroredMonoType = Lst[?];
    type MirroredElemTypes[_] <: Tuple
  }

  type OfCs = deriving.Mirror {
    type MirroredType[X] = Cs[X];
    type MirroredMonoType = Cs[?];
    type MirroredElemTypes[_] <: Tuple
  }

  val M_Lst = summon[OfLst]
  val M_Cs = summon[OfCs]

  val buf = new mutable.ListBuffer[Int]

  implicit def pklLst[T: Pickler]: Pickler[Lst[T]] = Pickler.derived
  val pklLstInt = pklLst[Int]
  val lst = Cs(1, Cs(2, Cs(3, Nl)))
  pklLstInt.pickle(buf, lst)
  val lst1 = pklLstInt.unpickle(buf)
  assert(lst == lst1)

  val Food = Json[{
    val kind: JsonString
    val isSandwich: JsonBoolean
    val brand: JsonString | JsonNull.type
  }]

  val Person = Json[{
    val name: JsonString
    val age: JsonNumber
    val favFoods: JsonArray[Food.T]
  }]

  val People: JsonArray[Person.T] = json(
    Person(
      "name" -> JsonString("Alice"),
      "age" -> JsonNumber(25),
      "favFoods" -> json(
        Food(
          "kind" -> JsonString("Pizza"),
          "isSandwich" -> JsonFalse,
        ),
        Food(
          "kind" -> JsonString("Hotdog"),
          "isSandwich" -> JsonTrue, // lol
          "brand" -> JsonString("Coop")
        )
      )
    )
  )

  implicit def pklJson[T <: Json[Any]]: Pickler[T] = Pickler.derived
  val pklPeople = pklJson[JsonArray[Person.T]]
  pklPeople.pickle(buf, People)
  val People1 = pklPeople.unpickle(buf)
  assert(People == People1)
  assert(People1(0).name.str == "Alice")
  assert(People1(0).age.num == 25.0)
  assert(People1(0).favFoods(0).kind.str == "Pizza")
  assert(People1(0).favFoods(0).isSandwich == JsonFalse)
  assert(People1(0).favFoods(0).brand == JsonNull)
  assert(People1(0).favFoods(1).kind.str == "Hotdog")
  assert(People1(0).favFoods(1).isSandwich == JsonTrue)
  assert(People1(0).favFoods(1).brand == JsonString("Coop"))
}
