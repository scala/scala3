import Inspect.*

object Test extends App {
  inspect[scala.collection.immutable.List[Int]]
  inspect[java.lang.String]
  inspect[String]
  inspect[List[Unit]]
  inspect[Some[Unit]]
  inspect[Tuple1[Unit]]
}
