 // https://github.com/scala/scala3/issues/23487
import java.io.*
import scala.annotation.threadUnsafe

def serialize[T <: Serializable](obj: T): Array[Byte] = {
  val byteArrayOutputStream = new ByteArrayOutputStream()
  val objectOutputStream    = new ObjectOutputStream(byteArrayOutputStream)

  try {
    objectOutputStream.writeObject(obj)
    byteArrayOutputStream.toByteArray
  } finally {
    objectOutputStream.close()
    byteArrayOutputStream.close()
  }
}

def deserialize[T](bytes: Array[Byte]): T = {
  val byteArrayInputStream = new ByteArrayInputStream(bytes)
  val objectInputStream    = new ObjectInputStream(byteArrayInputStream)

  try {
    objectInputStream.readObject().asInstanceOf[T]
  } finally {
    objectInputStream.close()
    byteArrayInputStream.close()
  }
}

case class Foo() {
  @transient
  lazy val value: Long = System.nanoTime()
}

case class Bar() {
  @transient @threadUnsafe
  lazy val value: Long = System.nanoTime()
}

@main def Test() = {
  val foo1 = Foo()
  foo1.value // init lazy val
  val foo2 = deserialize[Foo](serialize(foo1))
  assert(foo1.value != foo2.value, "Foo#value is not transient")

  val bar1 = Bar()
  bar1.value // init lazy val
  val bar2 = deserialize[Bar](serialize(bar1))
  assert(bar1.value != bar2.value, "Bar#value is not transient")
}