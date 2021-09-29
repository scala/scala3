class ClassWithLambda(sup: () => Long)
class ClassWithVar(var msg: String) extends ClassWithLambda(() => 1)

object Test:
  val _ = new ClassWithVar("foo")

  def main(args: Array[String]): Unit = {
    println("it worked")
  }