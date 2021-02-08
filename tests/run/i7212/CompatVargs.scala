import scala.annotation.*

class CompatVargs {
  @varargs
  def vargs(args: String*): Unit = println(args)

  def vargsFromScala(): Unit =
    vargs("single")
    vargs("a", "b")
    vargs(Seq("a", "b")*)
}
