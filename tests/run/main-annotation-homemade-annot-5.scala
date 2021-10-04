import scala.annotation.*
import scala.util.CommandLineParser.FromString

@mainManyArgs(Some(1)) def foo() = println("Hello world!")
@mainManyArgs(None) def bar() = println("Hello world!")

object Test:
  def main(args: Array[String]) =
    for (methodName <- List("foo", "bar"))
      val clazz = Class.forName(methodName)
      val method = clazz.getMethod("main", classOf[Array[String]])
      method.invoke(null, Array[String]())
end Test

@experimental
class mainManyArgs(o: Option[Int]) extends MainAnnotation:
  import MainAnnotation.*

  def command(info: CommandInfo, args: Array[String]): Command[FromString, Any] =
    new Command[FromString, Any]:
      override def argGetter[T](idx: Int, defaultArgument: Option[() => T])(using p: FromString[T]): () => T = ???

      override def varargGetter[T](using p: FromString[T]): () => Seq[T] = ???

      override def run(program: () => Any): Unit = program()
  end command
