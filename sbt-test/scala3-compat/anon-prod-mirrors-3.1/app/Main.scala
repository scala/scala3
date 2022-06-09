import scala.deriving.Mirror

package lib {
  object NewMirrors {
    val mFoo = summon[Mirror.Of[Foo]] // we can access the constructor of Foo here.
    val mFooObj = summon[Mirror.Of[Foo.type]]

    object SubBar extends Bar(1) {
      val mBar = summon[deriving.Mirror.ProductOf[Bar]]
      val mBarObj = summon[deriving.Mirror.ProductOf[Bar.type]]
    }
  }
}

package app {
  object Main:

    def testFoo(): Unit = {
      val oldMirrorFoo: Mirror.ProductOf[lib.Foo] = lib.OldMirrors.mFoo
      val oldMirrorFooObj: Mirror.ProductOf[lib.Foo.type] = lib.OldMirrors.mFooObj

      assert(oldMirrorFoo eq oldMirrorFooObj) // - not good as oldMirrorFoo is really the mirror for `Foo.type`
      assert(oldMirrorFooObj eq lib.Foo) // - object Foo is its own mirror

      // 3.1 bug: mirror for Foo behaves as mirror for Foo.type
      assert(oldMirrorFooObj.fromProduct(EmptyTuple) == lib.Foo)

      val newMirrorFoo: Mirror.ProductOf[lib.Foo] = lib.NewMirrors.mFoo
      val newMirrorFooObj: Mirror.ProductOf[lib.Foo.type] = lib.NewMirrors.mFooObj

      assert(oldMirrorFooObj eq newMirrorFooObj) // mirror for Foo.type has not changed.

      assert(newMirrorFoo ne lib.Foo) // anonymous mirror for Foo
      assert(newMirrorFoo.fromProduct(Tuple(23)).x == 23) // mirror for Foo behaves as expected
    }

    def testBar(): Unit = {
      val oldMirrorBar: Mirror.ProductOf[lib.Bar] = lib.OldMirrors.SubBar.mBar
      val oldMirrorBarObj: Mirror.ProductOf[lib.Bar.type] = lib.OldMirrors.SubBar.mBarObj

      assert(oldMirrorBar eq oldMirrorBarObj) // - not good as oldMirrorBar is really the mirror for `Bar.type`
      assert(oldMirrorBarObj eq lib.Bar) // - object Bar is its own mirror

      // 3.1 bug: mirror for Bar behaves as mirror for Bar.type
      assert(oldMirrorBarObj.fromProduct(EmptyTuple) == lib.Bar)

      val newMirrorBar: Mirror.ProductOf[lib.Bar] = lib.NewMirrors.SubBar.mBar
      val newMirrorBarObj: Mirror.ProductOf[lib.Bar.type] = lib.NewMirrors.SubBar.mBarObj

      assert(oldMirrorBarObj eq newMirrorBarObj) // mirror for Bar.type has not changed.

      assert(newMirrorBar ne lib.Bar) // anonymous mirror for Bar
      assert(newMirrorBar.fromProduct(Tuple(23)).x == 23) // mirror for Bar behaves as expected
    }

    def main(args: Array[String]): Unit =
      testFoo()
      testBar()
}
