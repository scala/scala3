import language.`3.7`
import reflect.ClassTag
import compiletime.ExpressibleAsCollectionLiteral
import collection.immutable.BitSet

/** Some delayed computation like a Mill Task */
case class Task[T](body: () => T)

object SeqLits:

  given [T] => ExpressibleAsCollectionLiteral[Task[Seq[T]]]:
    type Elem = T
    inline def fromLiteral(inline xs: T*): Task[Seq[T]] = Task(() => Seq(xs*))

  def last: Int = { println("last was evaluated"); 4 }

  @main def Test =
    val s: Seq[Int] = [1, 2, 3, last]
    val v: Vector[Int] = [1, 2, 3, last]
    val t: Task[Seq[Int]] = [1, 2, 3, last]
    val ve: Vector[String] = []
    val a: Array[String] = ["hello", "world"]
    val ia: IArray[String] = ["hello", "world"]
    val iae: IArray[String] = []
    val u = [1, 2, 3]
    val _: Seq[Int] = u
    val e = []
    val _: Seq[Int] = e
    val m = [1 -> "one", 2 -> "two"]
    val _: Map[Int, String] = m
    val bs: BitSet = [1, 2, 4, last]
    val ss: Seq[Seq[Int]] = [[1], [2, 3], []]
    val ss2 = [[1], [2, 3], []]
    val _: Seq[Seq[Int]] = ss2
    val vs: Vector[Vector[Int]] = [[1], [2, 3], []]

    println(s"Seq $s")
    println(s"Vector $v")
    println(bs)
    println(s"${t.getClass.getSimpleName} with elems ${t.body()}")
    println(ve)
    println(a.toList)
    println(ia.toList)
    println(iae.toList)
    println(u)
    println(e)
    println(m)
    println(ss)
    println(ss2)
    println(vs)
