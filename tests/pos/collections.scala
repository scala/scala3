import scala.collection.generic.CanBuildFrom

object collections {

  val arr = Array("a", "b")
  val aa = arr ++ arr

  List(1, 2, 3) map (x => 2)

  val s = Set(1, 2, 3)
  val ss = s map (_ + 1)

  val cbf: CanBuildFrom[List[_], Int, List[Int]] = scala.collection.immutable.List.canBuildFrom

  val nil = Nil
  val ints1 = 1 :: Nil
  val ints2 = 1 :: 2 :: Nil
  val ints3: List[Int] = ints2
  val f = (x: Int) => x + 1
  val ints4: List[Int] = List(1, 2, 3, 5)

  val ys = ints3 map (x => x + 1)
  val zs = ys filter (y => y != 0)

  val chrs = "abc"

  def do2(x: Int, y: Char) = ()

  chrs foreach println

  (ints2, chrs).zipped foreach do2

  val xs = List(List(1), List(2), List(3)).iterator
  println(/*scala.collection.TraversableOnce.flattenTraversableOnce*/(xs).flatten)

}
