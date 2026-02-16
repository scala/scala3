opaque type Namespace = List[String]

object Namespace:
  def apply(head: String): Namespace = List(head)

extension (ns: Namespace)
  def appended(segment: String): Namespace = ns.appended(segment)

object Main:
  def main(args: Array[String]): Unit =
    val a: Namespace = Namespace("a")
      .appended("B")
      .appended("c") // was error: Found: List[String] Required: Namespace
