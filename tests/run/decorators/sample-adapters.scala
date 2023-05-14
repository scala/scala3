// Sample adapters:

class logged extends EntryPoint.Adapter:

  def wrapper(wrapped: EntryPoint.Wrapper): LoggedWrapper = LoggedWrapper(wrapped)

  class LoggedWrapper(val wrapped: EntryPoint.Wrapper) extends Wrapper:
    def adapt[A, R](op: A => R)(args: A): R =
      val argsString: String = args match
        case args: Array[_] => args.mkString(", ")
        case args: Seq[_] => args.mkString(", ")
        case args: Unit => "()"
        case args => args.toString
      val result = op(args)
      val resultString: String = result match
        case result: Unit => "()"
        case _ => result.toString
      println(s"[log] ${finalWrapped.entryPointName}($argsString) -> $resultString")
      result
  end LoggedWrapper
end logged

class split extends EntryPoint.Adapter:

  def wrapper(wrapped: EntryPoint.Wrapper): SplitWrapper = SplitWrapper(wrapped)

  class SplitWrapper(val wrapped: EntryPoint.Wrapper) extends Wrapper:
    def adapt[R](op: Array[String] => R)(args: String): R = op(args.split(" "))
end split

class join extends EntryPoint.Adapter:

  def wrapper(wrapped: EntryPoint.Wrapper): JoinWrapper = JoinWrapper(wrapped)

  class JoinWrapper(val wrapped: EntryPoint.Wrapper) extends Wrapper:
    def adapt[R](op: String => R)(args: Array[String]): R = op(args.mkString(" "))
end join
