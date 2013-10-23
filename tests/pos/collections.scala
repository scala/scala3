import scala.collection.generic.CanBuildFrom

object collections {
  
  val cbf: CanBuildFrom[List, Int, List[Int]] = scala.collection.immutable.List.canBuildFrom

  val nil = Nil
  val ints1 = 1 :: Nil
  val ints2 = 1 :: 2 :: Nil
  val ints3: List[Int] = ints2
  val f = (x: Int) => x + 1

  val ys = ints3 map ((x: Int) => x + 1)

}