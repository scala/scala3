import collection.mutable

/** A sample @main entry point annotation.
 *  Generates a main function.
 */
class main extends EntryPoint.Annotation:

  type ArgumentParser[T] = util.CommandLineParser.FromString[T]
  type EntryPointResult  = Unit

  inline def wrapperName(entryPointName: String): String =
    s"${entryPointName.drop(entryPointName.lastIndexOf('.') + 1)}.main"

  def wrapper(name: String, doc: String): MainWrapper = new MainWrapper(name, doc)

  class MainWrapper(val entryPointName: String, val docComment: String) extends Wrapper:
    type Argument = Array[String]
    type Result = Unit

    def call(args: Array[String]) = new Call:

      /** A buffer of demanded argument names, plus
      *   "?"  if it has a default
      *   "*"  if it is a vararg
      *   ""   otherwise
      */
      private var argInfos = new mutable.ListBuffer[(String, String)]

      /** A buffer for all errors */
      private var errors = new mutable.ListBuffer[String]

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

      def nextArgGetter[T](argName: String, p: ArgumentParser[T], defaultValue: Option[T] = None): () => T =
        argInfos += ((argName, if defaultValue.isDefined then "?" else ""))
        val idx = args.indexOf(s"--$argName")
        val argOpt = if idx >= 0 then argAt(idx + 1) else nextPositionalArg()
        argOpt match
          case Some(arg) => convert(argName, arg, p)
          case None => defaultValue match
            case Some(t) => () => t
            case None => error(s"missing argument for $argName")

      def finalArgsGetter[T](argName: String, p: ArgumentParser[T]): () => Seq[T] =
        argInfos += ((argName, "*"))
        def remainingArgGetters(): List[() => T] = nextPositionalArg() match
          case Some(arg) => convert(argName, arg, p) :: remainingArgGetters()
          case None => Nil
        val getters = remainingArgGetters()
        () => getters.map(_())

      def run(entryPointWithArgs: => EntryPointResult): Unit =
        lazy val DocComment(explanation, docTags) = DocComment.fromString(docComment)

        def usageString =
          docTags.get("@usage") match
            case Some(s :: _) => s
            case _ =>
              val cmd = wrapperName(entryPointName).stripSuffix(".main")
              val params = argInfos.map(_ + _).mkString(" ")
              s"java $cmd $params"

        def printUsage() = println(s"Usage: $usageString")

        def explain(): Unit =
          if explanation.nonEmpty then println(explanation)
          printUsage()
          docTags.get("@param") match
            case Some(paramInfos) =>
              println("where")
              for paramInfo <- paramInfos do
                val ws = WordSplitter(paramInfo)
                val name = ws.next()
                val desc = paramInfo.drop(ws.nextOffset)
                println(s"  $name   $desc")
            case None =>
        end explain

        def flagUnused(): Unit = nextPositionalArg() match
          case Some(arg) =>
            error(s"unused argument: $arg")
            flagUnused()
          case None =>
            for
              arg <- args
              if arg.startsWith("--") && !argInfos.map(_._1).contains(arg.drop(2))
            do
              error(s"unknown argument name: $arg")
        end flagUnused

        if args.contains("--help") then
          explain()
        else
          flagUnused()
          if errors.nonEmpty then
            for msg <- errors do println(s"Error: $msg")
            printUsage()
            if explanation.nonEmpty || docTags.contains("@param") then
              println("--help gives more information")
          else entryPointWithArgs
      end run
    end call
  end MainWrapper
end main
