import java.io.{ByteArrayInputStream, ByteArrayOutputStream, ObjectInputStream, ObjectOutputStream, PrintWriter, StringWriter}
import java.lang.invoke.{MethodHandleInfo, SerializedLambda}

class C1 {
  val xxlfun = ((x1: Int,
    x2: String,
    x3: Int,
    x4: Int,
    x5: Int,
    x6: Int,
    x7: Int,
    x8: Int,
    x9: Int,
    x10: Int,
    x11: Int,
    x12: Int,
    x13: Int,
    x14: Int,
    x15: Int,
    x16: Int,
    x17: Int,
    x18: Int,
    x19: Int,
    x20: Int,
    x21: Int,
    x22: Int,
    x23: Int,
    x24: Int,
    x25: Int,
    x26: Int) => x2 + x1)

  val depfun: (x1: Int) => List[x1.type] = x1 => List(x1)

  val erasedfun: erased Int => Int = erased (x1) => 0
}

class C2
    extends Serializable /* Needed because of #5866 */ {
  val impfun: given Int => Int = given x1 => x1

  val impdepfun: given (x1: Int) => List[x1.type] = given x1 => List(x1)

  val erasedimpfun: given erased Int => Int = given erased (x1) => 0
}

object Test {
  def main(args: Array[String]): Unit = {
    val c1 = new C1
    serializeDeserialize(c1.xxlfun)
    serializeDeserialize(c1.erasedfun)

    val c2 = new C2
    serializeDeserialize[given Int => Int](c2.impfun)
    // Won't compile until #5841 is merged
    // serializeDeserialize[given Int => Int](c2.impdepfun)
    serializeDeserialize[given Int => Int](c2.erasedimpfun)
  }

  def serializeDeserialize[T <: AnyRef](obj: T): Unit = {
    val buffer = new ByteArrayOutputStream
    val out = new ObjectOutputStream(buffer)
    out.writeObject(obj)
    val in = new ObjectInputStream(new ByteArrayInputStream(buffer.toByteArray))
    in.readObject.asInstanceOf[T]
  }
}

