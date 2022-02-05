import scala.concurrent._
import scala.annotation.MainAnnotation
import scala.collection.mutable
import ExecutionContext.Implicits.global
import duration._

@mainAwait def get(wait: Int): Future[Int] = Future{
  Thread.sleep(1000 * wait)
  42
}

object Test:
  def callMain(args: Array[String]): Unit =
    val clazz = Class.forName("get")
    val method = clazz.getMethod("main", classOf[Array[String]])
    method.invoke(null, args)

  def main(args: Array[String]): Unit =
    println(Await.result(get(1), Duration(2, SECONDS)))
    callMain(Array("1"))
end Test

class mainAwait(timeout: Int = 2) extends MainAnnotation:
  import MainAnnotation._

  override type ArgumentParser[T] = util.CommandLineParser.FromString[T]
  override type MainResultType = Future[Any]

  // This is a toy example, it only works with positional args
  override def command(args: Array[String], commandName: String, docComment: String, parameterInfos: MainAnnotation.ParameterInfos*) =
    new Command[ArgumentParser, MainResultType]:
      private var idx = 0

      override def argGetter[T](name: String, optDefaultGetter: Option[() => T])(using p: ArgumentParser[T]): () => T =
        val i = idx
        idx += 1
        () => p.fromString(args(i))

      override def varargGetter[T](name: String)(using p: ArgumentParser[T]): () => Seq[T] =
        () => for i <- (idx until args.length) yield p.fromString(args(i))

      override def run(f: => MainResultType): Unit = println(Await.result(f, Duration(timeout, SECONDS)))
end mainAwait