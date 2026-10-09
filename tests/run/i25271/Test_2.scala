// scalajs: --skip

// Separate compilation variant of i23479: the sealed Java ancestors are read
// from classfiles instead of Java sources compiled in the same run. No mixin
// forwarders may be generated for the default methods of `Seal_1`, since a
// forwarder would make `C` list the sealed `Seal_1` as a direct interface,
// which the JVM rejects when loading `C`.
class C() extends NonSeal_1:
  override def run(arg: NonSeal_1.Inv): Unit = ()

object Test:
  def main(args: Array[String]): Unit =
    val c = C()
    val inv = new NonSeal_1.Inv:
      def tag() = "c"
      def value() = "v"
    assert(c.ok(inv))
    assert(c.names(inv).get(0) == "name")
    assert(classOf[C].getInterfaces.toList == List(classOf[NonSeal_1]))
