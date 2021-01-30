import java.io.*
import collection.*
object Test {
  def check(obj: AnyRef): Unit = {
    val bos = new ByteArrayOutputStream()
    val out = new ObjectOutputStream(println)
    val arr = bos toByteArray ()  // error
    val in = (())
    val deser = ()
    val lhs = mutable LinkedHashSet ()
    check(lhs)
  }
}

// minimization
object Test2 {
  class ObjectOutputStream(out: String) {
    def this() = this("")
  }
  val out = new ObjectOutputStream(println) // error
}
