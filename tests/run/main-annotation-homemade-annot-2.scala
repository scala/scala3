import scala.collection.mutable
import scala.annotation.MainAnnotation

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
class myMain(runs: Int = 3)(after: String*) extends MainAnnotation:
  import MainAnnotation._

  override type ArgumentParser[T] = util.CommandLineParser.FromString[T]
  override type MainResultType = Any

  override def command(args: Array[String], commandName: String, docComment: String, parameterInfos: MainAnnotation.ParameterInfos*) =
    new Command[ArgumentParser, MainResultType]:
      private var idx = 0

      override def argGetter[T](name: String, optDefaultGetter: Option[() => T])(using p: ArgumentParser[T]): () => T =
        val i = idx
        idx += 1
        () => p.fromString(args(i))

      override def varargGetter[T](name: String)(using p: ArgumentParser[T]): () => Seq[T] =
        () => for i <- (idx until args.length) yield p.fromString(args(i))

      override def run(f: => MainResultType): Unit =
        for (_ <- 1 to runs)
          f
          if after.length > 0 then println(after.mkString(", "))
      end run
  end command
end myMain