// scalajs: --skip

case class Foo(s: String)

object Test {
  def main(args: Array[String]): Unit = {
    val foo = Foo("bar")
    assert(foo.isInstanceOf[Serializable])
    assert(SerDes.serializeDeserialize[Foo](foo) == foo)
  }
}

// From https://github.com/scala/scala/pull/5278
object SerDes {
  import java.io.*

  def assertNotSerializable(a: AnyRef): Unit = {
    try {
      serialize(a)
      assert(false)
    } catch {
      case _: NotSerializableException => // okay
    }
  }

  def serialize(obj: AnyRef): Array[Byte] = {
    val buffer = new ByteArrayOutputStream
    val out = new ObjectOutputStream(buffer)
    out.writeObject(obj)
    buffer.toByteArray
  }

  def deserialize(a: Array[Byte]): AnyRef = {
    val in = new ObjectInputStream(new ByteArrayInputStream(a))
    in.readObject
  }

  def serializeDeserialize[T <: AnyRef](obj: T) = deserialize(serialize(obj)).asInstanceOf[T]
}
