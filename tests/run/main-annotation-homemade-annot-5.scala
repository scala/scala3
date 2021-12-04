import scala.annotation.MainAnnotation

@mainManyArgs(Some[Int](1)) def foo() = println("Hello world!")
@mainManyArgs(None) def bar() = println("Hello world!")

object Test:
  def main(args: Array[String]) =
    for (methodName <- List("foo", "bar"))
      val clazz = Class.forName(methodName)
      val method = clazz.getMethod("main", classOf[Array[String]])
      method.invoke(null, Array[String]())
end Test

class mainManyArgs(o: Option[Int]) extends MainAnnotation:
  override type ArgumentParser[T] = util.CommandLineParser.FromString[T]
  override type MainResultType = Any

  override def command(args: Array[String], commandName: String, docComment: String) =
    new MainAnnotation.Command[ArgumentParser, MainResultType]:
      override def argGetter[T](paramInfos: MainAnnotation.ParameterInfos[T])(using p: ArgumentParser[T]): () => T = ???

      override def varargGetter[T](paramInfos: MainAnnotation.ParameterInfos[T])(using p: ArgumentParser[T]): () => Seq[T] = ???

      override def run(f: => MainResultType): Unit = f
  end command