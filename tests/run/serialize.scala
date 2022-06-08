package a {
  object Outer extends Serializable {
    private object Inner extends Serializable

    val inner: AnyRef = Inner
  }
  class Bar extends Serializable {
    val x: AnyRef = Outer.inner
  }
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

  object Foo extends Serializable {}

  object Baz extends Serializable {
    private def writeReplace(): AnyRef = {
      this
    }
  }

  // No Companion defined - therefore anonmymous mirror is generated
  sealed trait NoCompanion
  case class Value(value: String) extends NoCompanion

  def main(args: Array[String]): Unit = {
    val x: PartialFunction[Int, Int] = { case x => x + 1 }
    val adder = serializeDeserialize(x)
    assert(adder(1) == 2)

    val foo = serializeDeserialize(Foo)
    assert(foo eq Foo)

    val baz = serializeDeserialize(Baz)
    assert(baz ne Baz)

    val bar = new a.Bar
    val bar1 = serializeDeserialize(bar)
    assert(bar.x eq bar1.x)

    val mirror = summon[scala.deriving.Mirror.Of[NoCompanion]]
    val mirror1 = serializeDeserialize(mirror)
    assert(mirror ne mirror1) // update if we start caching anonymous mirrors
    assert(mirror1.ordinal(Value("")) == 0) // check API
  }
}
