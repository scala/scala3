import scala.collection.mutable
import scala.annotation.*
import util.CommandLineParser.FromString

@myMain()("A")
def foo1(): Unit = println("I was run!")

@myMain(0)("This should not be printed")
def foo2() = throw new Exception("This should not be run")

@myMain(1)("Purple smart", "Blue fast", "White fashion", "Yellow quiet", "Orange honest", "Pink loud")
def foo3() = println("Here are some colors:")

@myMain()()
def foo4() = println("This will be printed, but nothing more.")

object Test:
  val allClazzes: Seq[Class[?]] =
    LazyList.from(1).map(i => scala.util.Try(Class.forName("foo" + i.toString))).takeWhile(_.isSuccess).map(_.get)

  def callMains(): Unit =
    for (clazz <- allClazzes)
      val method = clazz.getMethod("main", classOf[Array[String]])
      method.invoke(null, Array[String]())

  def main(args: Array[String]) =
    callMains()
end Test

// This is a toy example, it only works with positional args
@experimental
class myMain(runs: Int = 3)(after: String*) extends MainAnnotation:
  import MainAnnotation.*

  def command(info: CommandInfo, args: Array[String]): Command[FromString, Any] =
    new Command[FromString, Any]:

      override def argGetter[T](idx: Int, defaultArgument: Option[() => T])(using p: FromString[T]): () => T =
        () => p.fromString(args(idx))

      override def varargGetter[T](using p: FromString[T]): () => Seq[T] =
        () => for i <- (info.parameters.length until args.length) yield p.fromString(args(i))

      override def run(f: () => Any): Unit =
        for (_ <- 1 to runs)
          f()
          if after.length > 0 then println(after.mkString(", "))
      end run
  end command
end myMain
