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
class mainManyArgs(i1: Int, s2: String, i3: Int) extends MainAnnotation[FromString, Any]:
  import MainAnnotation.*

  def command(info: Info, args: Seq[String]): Option[Seq[String]] = Some(args)

  def argGetter[T](param: Parameter, arg: String, defaultArgument: Option[() => T])(using p: FromString[T]): () => T = ???

  def varargGetter[T](param: Parameter, args: Seq[String])(using p: FromString[T]): () => Seq[T] = ???


  def run(program: () => Any): Unit = program()
