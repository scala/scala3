//> using options -Werror
extension (sc: StringContext)
  def m: StringContext = sc
  def unapply(string: String): Option[String] =
    val pattern = sc.parts.head
    if string.length == pattern.length then Some(string) else None

class Test:
  def parse(x: PartialFunction[String, String]) = x

  val pf = parse {
    case m"x$s"  => s
    case m"xx$s" => s // was: unreachable
  }

  // proof that the second case isn't unreachable (matches "ab")
  def t1 = pf.applyOrElse("a", _ => ".") // "a"
  def t2 = pf.applyOrElse("ab", _ => ".") // "ab"
  def t3 = pf.applyOrElse("abc", _ => ".") // "."
