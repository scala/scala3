import language.`3.7`
import reflect.ClassTag
import compiletime.ExpressibleAsCollectionLiteral
import collection.immutable.BitSet
import collection.mutable.{ArrayBuffer, HashMap}
import language.experimental.collectionLiterals

/** Some delayed computation like a Mill Task */
case class Task[T](body: () => T)

object SeqLits:

  given [T] => ExpressibleAsCollectionLiteral[Task[Seq[T]]]:
    type Elem = T
    inline def fromLiteral(inline xs: T*): Task[Seq[T]] = Task(() => Seq(xs*))

  def last: Int = { println("last was evaluated"); 4 }

  def f1[A](xs: A, ys: A) = ys
  def f2[A](xs: Vector[A]) = xs

  def g[A](xs: Set[A]): Set[A] = xs
  def g[A](xs: collection.immutable.HashSet[A]): Set[A] = xs

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
    val ab: ArrayBuffer[Set[String]] = [["hello", "world"], []]
    val sbs: Set[BitSet] = [[1], [2, 3], []]
    val mbs: Map[Int, BitSet] = [1 -> [1], 2 -> [1, 2], 0 -> []]
    val hbs: HashMap[Int, Seq[BitSet]] = [1 -> [[1], [2, 3]], 2 -> [[]], 0 -> []]
    // val mbss: Map[BitSet, Seq[Int]] =  [[1] -> [1], [0, 2] -> [1, 2], [0] -> []]  // error: keys get default value Seq
    val mbss: Map[BitSet, Seq[Int]] =  [([1], [1]), ([0, 2], [1, 2]), ([0], [])] // ok

    val x1 = f1(Vector(1, 2, 3), [3, 4, 5])
    val _: Seq[Int] = x1
    val x2 = f2([1, 2, 3])
    val _: Vector[Int] = x2

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
    println(ab)
    println(sbs)
    println(mbs)
    println(hbs)
    println(mbss)


    val oneTwoThree: Vector[Int] = [1, 2, 3]
    val anotherLit: IArray[Double] = [math.Pi, math.cos(2.0), math.E * 3.0]
    val diag: Array[Array[Int]] = [[1, 0, 0], [0, 1, 0], [0, 0, 1]]
    val empty: ArrayBuffer[Object] = []
    val mapy: HashMap[Int, String] = [1 -> "one", 2 -> "two", 3 -> "three"]
