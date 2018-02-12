object Test {

  class Seq[T]
  class List[T] extends Seq[T] { def head: T = ??? }

  val ss: Seq[Int] = ???
  ss match {
    case ss: List[int] =>
      val x = ss.head
      val y: Int = x
  }
}
object Test1 {

  class Seq[T]
  class List[T] extends Seq[T] { def head: T = ??? }

  val ss: Seq[Int] = ???
  ss match {
    case ss: List[Int] =>
      println(ss)
  }
}
object Test2 {

  trait A
  trait B
  val x: A & B = ???

  (x: A) match {
    case x: B =>
  }
}
