class Outer {
  type Smuggler
  var smuggler: Option[Smuggler] = None
}
class Foo[T](var unpack: T)
class Evil(val outer: Outer, extract: outer.type => Unit) extends Foo[outer.type](outer) { // error
  def doExtract(): Unit = extract(unpack)
}

object Test {
  def main(args: Array[String]): Unit = {
    val outer1 = new Outer { type Smuggler = Int }
    outer1.smuggler = Some(5)
    val evil1 = new Evil(outer1, _ => ())

    val outer2 = new Outer { type Smuggler = String }
    var extractedOuter2: Option[outer2.type] = None
    val evil2 = new Evil(outer2, x => extractedOuter2 = Some(x))

    evil2.unpack = evil1.unpack
    evil2.doExtract()
    val smuggled: String = extractedOuter2.get.smuggler.get
    println(smuggled)
  }
}
