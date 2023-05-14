// scalajs: --skip

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
class mainAwait(timeout: Int = 2) extends MainAnnotation[FromString, Future[Any]]:
  import MainAnnotation.*

  // This is a toy example, it only works with positional args
  def command(info: Info, args: Seq[String]): Option[Seq[String]] = Some(args)

  def argGetter[T](param: Parameter, arg: String, defaultArgument: Option[() => T])(using p: FromString[T]): () => T =
    () => p.fromString(arg)

  def varargGetter[T](param: Parameter, args: Seq[String])(using p: FromString[T]): () => Seq[T] =
    () => for arg <- args yield p.fromString(arg)

  def run(f: () => Future[Any]): Unit = println(Await.result(f(), Duration(timeout, SECONDS)))

end mainAwait
