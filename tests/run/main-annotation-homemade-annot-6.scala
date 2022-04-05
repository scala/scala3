import scala.annotation.*

/** Foo docs */
@myMain def foo(i: Int, j: String = "2") = println(s"foo($i, $j)")
/** Bar docs
 *
 *  @param i the first parameter
 */
@myMain def bar(@MyParamAnnot(3) i: List[Int], rest: Int*) = println(s"bar($i, ${rest.mkString(", ")})")

object Test:
  def main(args: Array[String]) =
    for (methodName <- List("foo", "bar"))
      val clazz = Class.forName(methodName)
      val method = clazz.getMethod("main", classOf[Array[String]])
      method.invoke(null, Array[String]("1", "2"))
end Test

@experimental
class myMain extends MainAnnotation[Make, Any]:
  import MainAnnotation.*

  def command(info: Info, args: Seq[String]): Option[Seq[String]] =
    def paramInfoString(paramInfo: Parameter) =
      import paramInfo.*
      s"    Parameter(name=\"$name\", typeName=\"$typeName\", hasDefault=$hasDefault, isVarargs=$isVarargs, documentation=\"$documentation\", annotations=$annotations)"
    println(
      s"""command(
         |  ${args.mkString("Array(", ", ", ")")},
         |  ${info.name},
         |  "${info.documentation}",
         |  ${info.parameters.map(paramInfoString).mkString("Seq(\n", ",\n", "\n  )*")}
         |)""".stripMargin)
    Some(args)

  def argGetter[T](param: Parameter, arg: String, defaultArgument: Option[() => T])(using p: Make[T]): () => T =
    () => p.make

  def varargGetter[T](param: Parameter, args: Seq[String])(using p: Make[T]): () => Seq[T] =
    println("varargGetter()")
    () => Seq(p.make, p.make)

  def run(f: () => Any): Unit =
    println("run()")
    f()
    println()

@experimental
case class MyParamAnnot(n: Int) extends MainAnnotation.ParameterAnnotation

trait Make[T]:
  def make: T

given Make[Int] with
  def make: Int = 42


given Make[String] with
  def make: String = "abc"

given [T: Make]: Make[List[T]] with
  def make: List[T] = List(summon[Make[T]].make)
