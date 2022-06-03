import java.util.{List, ArrayList}

object ForEachDesugar {

  val listA = scala.List(1, 2, 3)
  val listB = List.of(1, 2, 3)
  val listC = ArrayList[Int]()

  for i <- listB do listB.add(i)

  for
    x <- listA
    y <- listB
    z <- listC
  do
    listB.add(x + y)

}