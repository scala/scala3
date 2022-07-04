// scalajs: --skip

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
class mainManyArgs(o: Option[Int]) extends MainAnnotation[FromString, Any]:
  import MainAnnotation.*

  def command(info: Info, args: Seq[String]): Option[Seq[String]] = Some(args)

  def argGetter[T](param: Parameter, arg: String, defaultArgument: Option[() => T])(using p: FromString[T]): () => T = ???

  def varargGetter[T](param: Parameter, args: Seq[String])(using p: FromString[T]): () => Seq[T] = ???

  def run(program: () => Any): Unit = program()
