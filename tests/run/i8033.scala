// scalajs: --skip

trait Okay extends Serializable {
  def okay: Okay
}

class Foo {
  def okay1: Okay = new Okay() {
    val okay: Okay = this
    override def toString = "okay1"
  }
  def okay2: Okay = new Okay {
    val okay: Okay = okay1
    override def toString = "okay2"
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    val foo = new Foo
    assert(roundTrip(foo.okay1).toString == "okay1")
    assert(roundTrip(foo.okay2).toString == "okay2")
  }

  def roundTrip[A](a: A): A = {
    import java.io.*

    val aos = new ByteArrayOutputStream()
    val oos = new ObjectOutputStream(aos)
    oos.writeObject(a)
    oos.close()
    val ais = new ByteArrayInputStream(aos.toByteArray())
    val ois: ObjectInputStream = new ObjectInputStream(ais)
    val newA = ois.readObject()
    ois.close()
    newA.asInstanceOf[A]
  }
}