import scala.concurrent._
import scala.annotation.*
import scala.collection.mutable
import ExecutionContext.Implicits.global
import duration._
import util.CommandLineParser.FromString

@mainAwait def get(wait: Int): Future[Int] = Future{
  Thread.sleep(1000 * wait)
  42
}

@mainAwait def getMany(wait: Int*): Future[Int] = Future{
  Thread.sleep(1000 * wait.sum)
  wait.length
}

object Test:
  def callMain(cls: String, args: Array[String]): Unit =
    val clazz = Class.forName(cls)
    val method = clazz.getMethod("main", classOf[Array[String]])
    method.invoke(null, args)

  def main(args: Array[String]): Unit =
    println(Await.result(get(1), Duration(2, SECONDS)))
    callMain("get", Array("1"))
    callMain("getMany", Array("1"))
    callMain("getMany", Array("0", "1"))
end Test

@experimental
class mainAwait(timeout: Int = 2) extends MainAnnotation:
  import MainAnnotation.*

  // This is a toy example, it only works with positional args
  def command(info: CommandInfo, args: Array[String]): Command[FromString, Future[Any]] =
    new Command[FromString, Future[Any]]:
      override def argGetter[T](idx: Int, defaultArgument: Option[() => T])(using p: FromString[T]): () => T =
        () => p.fromString(args(idx))

      override def varargGetter[T](using p: FromString[T]): () => Seq[T] =
        () => for i <- ((info.parameters.length-1) until args.length) yield p.fromString(args(i))

      override def run(f: () => Future[Any]): Unit = println(Await.result(f(), Duration(timeout, SECONDS)))
end mainAwait
