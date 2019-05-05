object Test {

  type YZ = [Y[_]] => [Z[_]] => [T] => Y[Z[T]]

  val r1: List[List[Int]] = ???
  def r2(): List[List[Int]] = ???

  val l1: YZ[List][List][Int] = r1
  val l2: YZ[List][List][Int] = r2()
}