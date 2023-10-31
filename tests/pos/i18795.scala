package example

object Main extends App with Test {
  load("")()
}

trait Test {

  def load[T](
    doLoad: T
  )(
    description: T => Option[String] = (x: T) => None // <--- compile error here
  ): Unit = ???

}