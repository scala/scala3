//> using options -Werror
object DiffUtil:
  private sealed trait Patch
  private final case class Unmodified(str: String) extends Patch
  private final case class Modified(original: String, str: String) extends Patch
  private final case class Deleted(str: String) extends Patch
  private final case class Inserted(str: String) extends Patch

  private def test(diff: Array[Patch]) =
    diff.collect {
      case Unmodified(str)     => str
      case Inserted(str)       => s"+$str"
      case Modified(orig, str) => s"{$orig,$str}"
      case Deleted(str)        => s"-$str"
    }.mkString
