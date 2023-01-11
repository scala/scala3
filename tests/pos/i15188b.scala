class C

extension (ctx: C) inline def unapply(input: String): Option[String] = Some("hi")

@main def run = {
  val O = new C
  val O(x) = "3": @unchecked
}
