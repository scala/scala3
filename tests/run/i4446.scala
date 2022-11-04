// scalajs: --skip

class Foo {
  def foo: PartialFunction[Int, Int] = { case x => x + 1 }
}

object Test {
  def serializeDeserialize[T <: AnyRef](obj: T): T = {
    import java.io.*
    val buffer = new ByteArrayOutputStream
    val out = new ObjectOutputStream(buffer)
    out.writeObject(obj)
    val in = new ObjectInputStream(new ByteArrayInputStream(buffer.toByteArray))
    in.readObject.asInstanceOf[T]
  }

  def main(args: Array[String]): Unit = {
    val adder = serializeDeserialize((new Foo).foo)
    assert(adder(1) == 2)
  }
}
