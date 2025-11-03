import scala.collection.SortedSet
import scala.collection.immutable.{SortedSet => ISortedSet}
import scala.collection.mutable.{SortedSet => MSortedSet}

@main def Test(): Unit =
  val s1: SortedSet[Int] = SortedSet(1, 2, 3)
  println(s1)
  val s2 = s1.filter(_ % 2 == 1)
  println(s2)

  val s3: ISortedSet[Int] = ISortedSet(1, 2, 3)
  println(s3)
  val s4 = s3.filter(_ % 2 == 1)
  println(s4)

  val s5: MSortedSet[Int] = MSortedSet(4, 5, 6)
  println(s5)
  val s6 = s5.filter(_ % 2 == 0)
  println(s6)