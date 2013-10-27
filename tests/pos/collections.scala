import scala.collection.generic.CanBuildFrom

object collections {
  
  val cbf: CanBuildFrom[List, Int, List[Int]] = scala.collection.immutable.List.canBuildFrom

  val nil = Nil
  val ints1 = 1 :: Nil
  val ints2 = 1 :: 2 :: Nil
  val ints3: List[Int] = ints2
  val f = (x: Int) => x + 1
  val ints4: List[Int] = List(1, 2, 3, 5)
    
  val ys = ints3 map (x => x + 1)
  val zs = ys filter (y => y != 0)

}