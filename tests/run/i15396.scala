// scalajs: --skip

import java.io.*

class Foo extends Serializable:
  enum Bar:
    case A
    case B

  def matchBar(b: Bar): String = b match
    case Bar.A => "a"
    case Bar.B => "b"

class Foo2 extends Serializable:
  enum Baz:
    case X
    case Y(value: Int)

object Wrapper:
  enum Status:
    case Active, Inactive

trait MyTrait extends Serializable:
  enum Kind:
    case X, Y

class MyTraitImpl extends MyTrait

enum TopLevel:
  case One, Two, Three

def serializeDeserialize[T <: AnyRef](obj: T): T =
  val buffer = new ByteArrayOutputStream
  val out = new ObjectOutputStream(buffer)
  out.writeObject(obj)
  out.close()
  val in = new ObjectInputStream(new ByteArrayInputStream(buffer.toByteArray))
  in.readObject.asInstanceOf[T]

@main def Test =
  // --- inner enum: individual value serialization ---
  val foo = new Foo()

  val a = serializeDeserialize(foo.Bar.A)
  assert(a ne null, "deserialized A should not be null")
  assert(a.ordinal == 0, s"deserialized A should have ordinal 0, got ${a.ordinal}")
  assert(a.toString == "A", s"deserialized A should have name A, got ${a.toString}")

  val b = serializeDeserialize(foo.Bar.B)
  assert(b ne null, "deserialized B should not be null")
  assert(b.ordinal == 1, s"deserialized B should have ordinal 1, got ${b.ordinal}")
  assert(b.toString == "B", s"deserialized B should have name B, got ${b.toString}")

  // --- inner enum: whole outer instance serialization ---
  val foo2 = serializeDeserialize(foo)
  assert(foo2 ne null, "deserialized Foo should not be null")
  assert(foo2.Bar.A.ordinal == 0, "foo2.Bar.A should have ordinal 0")
  assert(foo2.Bar.B.ordinal == 1, "foo2.Bar.B should have ordinal 1")

  // --- inner enum: identity semantics ---
  assert(foo2.Bar.A eq foo2.Bar.A, "singleton identity should hold within same outer instance")
  assert(foo.Bar.A ne foo2.Bar.A, "deserialized value is a different object from original")

  // --- inner enum: pattern matching on deserialized values ---
  assert(foo2.matchBar(foo2.Bar.A) == "a", "pattern match works within deserialized graph (A)")
  assert(foo2.matchBar(foo2.Bar.B) == "b", "pattern match works within deserialized graph (B)")

  val fooTest = foo2.Bar.A match {
    case foo2.Bar.A => "a"
    case foo2.Bar.B => "b"
  }
  assert(fooTest == "a", "pattern match works within deserialized graph (A)")

  // --- inner enum: companion serialization ---
  val barCompanion = serializeDeserialize(foo.Bar)
  assert(barCompanion ne null, "deserialized companion should not be null")
  assert(barCompanion.A.ordinal == 0, "deserialized companion has correct A ordinal")
  assert(barCompanion.B.ordinal == 1, "deserialized companion has correct B ordinal")
  assert(!(barCompanion.A == foo2.Bar.A), "values from different companions are not equal")
  assert(barCompanion.values.map(_.ordinal).toList == List(0, 1),
    "deserialized companion values have correct ordinals")
  assert(barCompanion.values.map(_.toString).toList == List("A", "B"),
    "deserialized companion values have correct names")

  // --- inner enum: class case with parameter ---
  val foo2Instance = new Foo2()
  val x = serializeDeserialize(foo2Instance.Baz.X)
  assert(x ne null, "deserialized X should not be null")
  assert(x.ordinal == 0, s"deserialized X should have ordinal 0, got ${x.ordinal}")

  val y = serializeDeserialize(foo2Instance.Baz.Y(42))
  assert(y ne null, "deserialized Y should not be null")
  assert(y.ordinal == 1, s"deserialized Y should have ordinal 1, got ${y.ordinal}")
  assert(y.toString == "Y(42)", s"deserialized Y should preserve value, got ${y.toString}")

  // --- top-level enum: singleton identity preserved via readResolve ---
  val one = serializeDeserialize(TopLevel.One)
  assert(one eq TopLevel.One, "top-level enum should preserve singleton identity via readResolve")
  val two = serializeDeserialize(TopLevel.Two)
  assert(two eq TopLevel.Two, "top-level enum should preserve singleton identity via readResolve")
  val three = serializeDeserialize(TopLevel.Three)
  assert(three eq TopLevel.Three, "top-level enum should preserve singleton identity via readResolve")

  // --- enum inside object (static context): singleton identity preserved ---
  val active = serializeDeserialize(Wrapper.Status.Active)
  assert(active eq Wrapper.Status.Active, "enum inside object should preserve singleton identity")
  val inactive = serializeDeserialize(Wrapper.Status.Inactive)
  assert(inactive eq Wrapper.Status.Inactive, "enum inside object should preserve singleton identity")

  // --- enum inside trait (non-static context): should not crash ---
  val traitImpl = new MyTraitImpl()
  val kx = serializeDeserialize(traitImpl.Kind.X)
  assert(kx ne null, "deserialized trait enum value should not be null")
  assert(kx.ordinal == 0, s"deserialized trait enum X should have ordinal 0, got ${kx.ordinal}")
  val ky = serializeDeserialize(traitImpl.Kind.Y)
  assert(ky ne null, "deserialized trait enum value should not be null")
  assert(ky.ordinal == 1, s"deserialized trait enum Y should have ordinal 1, got ${ky.ordinal}")
