import scala.deriving.Mirror

class Outer3 { self =>

  object Inner {
    sealed trait Item { // both children and parent share a common sub-prefix

      final lazy val mItem = summon[Mirror.Of[Item]]
      final lazy val mA =
        type TA = Tuple.Elem[mItem.MirroredElemTypes, 0]
        summon[Mirror.Of[TA]]
      final lazy val mB =
        type TB = Tuple.Elem[mItem.MirroredElemTypes, 1]
        summon[Mirror.Of[TB]]
    }

    case class A() extends self.Inner.Item
    case object A // force anonymous mirror
    case class B() extends self.Inner.Item
  }

}

def testOuter3 =
  val o = Outer3()
  val a = o.Inner.A()
  val b = o.Inner.B()
  assert(b.mItem.ordinal(a) == 0)
  assert(b.mItem.ordinal(b) == 1)
  assert(b.mA.fromProduct(EmptyTuple) == a)
  assert(b.mB.fromProduct(EmptyTuple) == b)

class Outer4 {
  enum Item {
    case A, B
  }
}

def testOuter4 =
  val o = new Outer4()
  val mItem = summon[Mirror.Of[o.Item]]
  type derivedA = Tuple.Head[mItem.MirroredElemTypes]
  val mA = summon[Mirror.Of[derivedA]]
  assert(mItem.ordinal(o.Item.A) == 0)
  assert(mA.fromProduct(EmptyTuple) == o.Item.A)

class Outer5 { self =>

  sealed trait Item
  object Wrap {
    sealed trait Fruit extends Item
    object Fruit {
      case object Apple extends Item with Fruit
      case object Orange extends Item with Fruit
    }
  }

  final lazy val o = new Outer5() // infinite init

}

def testOuter5 =
  val o5 = new Outer5()
  val mFruit = summon[Mirror.Of[o5.o.Item & o5.o.Wrap.Fruit]]
  type derivedApple = Tuple.Head[mFruit.MirroredElemTypes]
  val mApple = summon[Mirror.Of[derivedApple]]

  assert(mFruit.ordinal(o5.o.Wrap.Fruit.Apple) == 0)
  assert(mApple.fromProduct(EmptyTuple) == o5.o.Wrap.Fruit.Apple)

class Outer6 {

  sealed trait Item
  object Wrap {
    case class Fruit(seed: Seed) extends Item
    case class Seed() extends Item
  }

  final lazy val o = new Outer6() // infinite init

  def hello: Unit = {

    val mFruit = summon[Mirror.Of[o.Item & o.Wrap.Fruit]]
    type derivedSeed = Tuple.Head[mFruit.MirroredElemTypes]
    val mSeed = summon[Mirror.Of[derivedSeed]]

    assert(mFruit.fromProduct(Tuple(o.Wrap.Seed())) == o.Wrap.Fruit(o.Wrap.Seed()))
    assert(mSeed.fromProduct(EmptyTuple) == o.Wrap.Seed()) // careful to ensure that correct outer is captured
  }

}

def testOuter6 = {
  val o = new Outer6()
  o.hello
}

def locally1 = {

  object Bar {
    case object A extends Foo.Item
  }

  object Foo {

    sealed trait Item {

      final lazy val mItem = summon[Mirror.Of[Item.this.type]]

    }
  }

  assert(Bar.A.mItem.ordinal(Bar.A) == 0)

}

def locally2: Unit = {

  sealed trait Item

  object Wrapper {
    case object A extends Item
    case object B extends Item
  }

  val mItem = summon[Mirror.Of[Item]]
  type derivedA = Tuple.Head[mItem.MirroredElemTypes]
  val mA = summon[Mirror.Of[derivedA]]

  assert(mItem.ordinal(Wrapper.A) == 0)
  assert(mA.fromProduct(EmptyTuple) == Wrapper.A)

}

def locally3 = {

  class Foo {
    final val foo: Qux.type = Qux
    case object Qux
  }

  class Bar extends Foo {

    def hello =
      val mQux = summon[Mirror.Of[foo.type]]
      assert(mQux.fromProduct(EmptyTuple) == Qux)
  }

  val b = new Bar()
  b.hello
}

@main def Test =
  testOuter3
  testOuter4
  testOuter5
  testOuter6
  locally1
  locally2
  locally3
