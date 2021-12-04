import scala.annotation.MainAnnotation

@mainManyArgs(1, "B", 3) def foo() = println("Hello world!")

object Test:
  def main(args: Array[String]) =
    val clazz = Class.forName("foo")
    val method = clazz.getMethod("main", classOf[Array[String]])
    method.invoke(null, Array[String]())
end Test

class mainManyArgs(i1: Int, s2: String, i3: Int) extends MainAnnotation:
  override type ArgumentParser[T] = util.CommandLineParser.FromString[T]
  override type MainResultType = Any

  override def command(args: Array[String], commandName: String, docComment: String) =
    new MainAnnotation.Command[ArgumentParser, MainResultType]:
      override def argGetter[T](paramInfos: MainAnnotation.ParameterInfos[T])(using p: ArgumentParser[T]): () => T = ???

      override def varargGetter[T](paramInfos: MainAnnotation.ParameterInfos[T])(using p: ArgumentParser[T]): () => Seq[T] = ???

      override def run(f: => MainResultType): Unit = f
  end command