import scala.annotation.*
import scala.util.CommandLineParser.FromString

@mainManyArgs(1, "B", 3) def foo() = println("Hello world!")

object Test:
  def main(args: Array[String]) =
    val clazz = Class.forName("foo")
    val method = clazz.getMethod("main", classOf[Array[String]])
    method.invoke(null, Array[String]())
end Test

@experimental
class mainManyArgs(i1: Int, s2: String, i3: Int) extends MainAnnotation:
  import MainAnnotation.*

  def command(info: CommandInfo, args: Array[String]): Command[FromString, Any] =
    new Command[FromString, Any]:
      override def argGetter[T](idx: Int, optDefaultGetter: Option[() => T])(using p: FromString[T]): () => T = ???

      override def varargGetter[T](using p: FromString[T]): () => Seq[T] = ???

      override def run(program: () => Any): Unit = program()
  end command
