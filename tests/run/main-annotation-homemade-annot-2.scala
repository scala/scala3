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

/** Code mostly copied from {{scala.main}}. */
class myMain(runs: Int = 3)(after: String*) extends MainAnnotation:
  self =>

  private val maxLineLength = 120

  override type ArgumentParser[T] = util.CommandLineParser.FromString[T]
  override type MainResultType = Any

  private enum ArgumentKind {
    case SimpleArgument, OptionalArgument, VarArgument
  }

  override def command(args: Array[String], commandName: String, docComment: String) =
    new MainAnnotation.Command[ArgumentParser, MainResultType]:
      private var argNames = new mutable.ArrayBuffer[String]
      private var argTypes = new mutable.ArrayBuffer[String]
      private var argDocs = new mutable.ArrayBuffer[String]
      private var argKinds = new mutable.ArrayBuffer[ArgumentKind]

      /** A buffer for all errors */
      private var errors = new mutable.ArrayBuffer[String]

      /** Issue an error, and return an uncallable getter */
      private def error(msg: String): () => Nothing =
        errors += msg
        () => throw new AssertionError("trying to get invalid argument")

      /** The next argument index */
      private var argIdx: Int = 0

      private def argAt(idx: Int): Option[String] =
        if idx < args.length then Some(args(idx)) else None

      private def nextPositionalArg(): Option[String] =
        while argIdx < args.length && args(argIdx).startsWith("--") do argIdx += 2
        val result = argAt(argIdx)
        argIdx += 1
        result

      private def convert[T](argName: String, arg: String, p: ArgumentParser[T]): () => T =
        p.fromStringOption(arg) match
          case Some(t) => () => t
          case None => error(s"invalid argument for $argName: $arg")

      private def argUsage(pos: Int): String =
        val name = argNames(pos)

        argKinds(pos) match {
          case ArgumentKind.SimpleArgument => s"[--$name] <${argTypes(pos)}>"
          case ArgumentKind.OptionalArgument => s"[[--$name] <${argTypes(pos)}>]"
          case ArgumentKind.VarArgument => s"[<${argTypes(pos)}> [<${argTypes(pos)}> [...]]]"
        }

      private def wrapLongLine(line: String, maxLength: Int): List[String] = {
        def recurse(s: String, acc: Vector[String]): Seq[String] =
          val lastSpace = s.trim.nn.lastIndexOf(' ', maxLength)
          if ((s.length <= maxLength) || (lastSpace < 0))
            acc :+ s
          else {
            val (shortLine, rest) = s.splitAt(lastSpace)
            recurse(rest.trim.nn, acc :+ shortLine)
          }

        recurse(line, Vector()).toList
      }

      private def wrapArgumentUsages(argsUsage: List[String], maxLength: Int): List[String] = {
        def recurse(args: List[String], currentLine: String, acc: Vector[String]): Seq[String] =
          (args, currentLine) match {
            case (Nil, "") => acc
            case (Nil, l) => (acc :+ l)
            case (arg :: t, "") => recurse(t, arg, acc)
            case (arg :: t, l) if l.length + 1 + arg.length <= maxLength => recurse(t, s"$l $arg", acc)
            case (arg :: t, l) => recurse(t, arg, acc :+ l)
          }

        recurse(argsUsage, "", Vector()).toList
      }

      private inline def shiftLines(s: Seq[String], shift: Int): String = s.map(" " * shift + _).mkString("\n")

      private def usage(): Unit =
        val usageBeginning = s"Usage: $commandName "
        val argsOffset = usageBeginning.length
        val argUsages = wrapArgumentUsages((0 until argNames.length).map(argUsage).toList, maxLineLength - argsOffset)

        println(usageBeginning + argUsages.mkString("\n" + " " * argsOffset))

      private def explain(): Unit =
        if (docComment.nonEmpty)
          println(wrapLongLine(docComment, maxLineLength).mkString("\n"))
        if (argNames.nonEmpty) {
          val argNameShift = 2
          val argDocShift = argNameShift + 2

          println("Arguments:")
          for (pos <- 0 until argNames.length)
            val argDoc = StringBuilder(" " * argNameShift)
            argDoc.append(s"${argNames(pos)} - ${argTypes(pos)}")

            argKinds(pos) match {
              case ArgumentKind.OptionalArgument => argDoc.append(" (optional)")
              case ArgumentKind.VarArgument => argDoc.append(" (vararg)")
              case _ =>
            }

            if (argDocs(pos).nonEmpty) {
              val shiftedDoc =
                argDocs(pos).split("\n").nn
                            .map(line => shiftLines(wrapLongLine(line.nn, maxLineLength - argDocShift), argDocShift))
                            .mkString("\n")
              argDoc.append("\n").append(shiftedDoc)
            }

            println(argDoc)
        }

      private def indicesOfArg(argName: String): Seq[Int] =
        def allIndicesOf(s: String): Seq[Int] =
          def recurse(s: String, from: Int): Seq[Int] =
            val i = args.indexOf(s, from)
            if i < 0 then Seq() else i +: recurse(s, i + 1)

          recurse(s, 0)

        val indices = allIndicesOf(s"--$argName")
        indices.filter(_ >= 0)

      private def getArgGetter[T](argName: String, getDefaultGetter: () => () => T)(using p: ArgumentParser[T]): () => T =
        indicesOfArg(argName) match {
          case s @ (Seq() | Seq(_)) =>
            val argOpt = s.headOption.map(idx => argAt(idx + 1)).getOrElse(nextPositionalArg())
            argOpt match {
              case Some(arg) => convert(argName, arg, p)
              case None => getDefaultGetter()
            }
          case s =>
            val multValues = s.flatMap(idx => argAt(idx + 1))
            error(s"more than one value for $argName: ${multValues.mkString(", ")}")
        }

      private def registerArg(argName: String, argType: String, argDoc: String, argKind: ArgumentKind): Unit =
        argNames += argName
        argTypes += argType
        argDocs += argDoc
        argKinds += argKind

      override def argGetter[T](argName: String, argType: String, argDoc: String)(using p: ArgumentParser[T]): () => T =
        registerArg(argName, argType, argDoc, ArgumentKind.SimpleArgument)
        getArgGetter(argName, () => error(s"missing argument for $argName"))

      override def argGetterDefault[T](argName: String, argType: String, argDoc: String, defaultValue: => T)(using p: ArgumentParser[T]): () => T =
        registerArg(argName, argType, argDoc, ArgumentKind.OptionalArgument)
        getArgGetter(argName, () => () => defaultValue)

      override def argsGetter[T](argName: String, argType: String, argDoc: String)(using p: ArgumentParser[T]): () => Seq[T] =
        registerArg(argName, argType, argDoc, ArgumentKind.VarArgument)
        def remainingArgGetters(): List[() => T] = nextPositionalArg() match
          case Some(arg) => convert(argName, arg, p) :: remainingArgGetters()
          case None => Nil
        val getters = remainingArgGetters()
        () => getters.map(_())

      override def run(f: => MainResultType): Unit =
        def flagUnused(): Unit = nextPositionalArg() match
          case Some(arg) =>
            error(s"unused argument: $arg")
            flagUnused()
          case None =>
            for
              arg <- args
              if arg.startsWith("--") && !argNames.contains(arg.drop(2))
            do
              error(s"unknown argument name: $arg")
        end flagUnused

        if args.contains("--help") then
          usage()
          println()
          explain()
        else
          flagUnused()
          if errors.nonEmpty then
            for msg <- errors do println(s"Error: $msg")
            usage()
          else
            for (_ <- 1 to runs)
              f
              if after.length > 0 then println(after.mkString(", "))
      end run
  end command
end myMain