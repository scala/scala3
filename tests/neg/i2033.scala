import java.io._
import collection._
object Test {
  def check(obj: AnyRef): Unit = {
    val bos = new ByteArrayOutputStream()
    val out = new ObjectOutputStream(println) // error
    val arr = bos toByteArray ()
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
