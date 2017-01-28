class Test {
  def byName(param: => Any) = {}

  def test: Unit = {

    // 1. In ResolveSuper#transformTemplate we add "def <(that: Int): Boolean =
    //    Foo.super[Ordered].<(that)", etc to Foo, these methods are inserted
    //    into a new decls scope using enteredAfter in MixinOps#implementation

    // 2. In ElimByName#transformApply we call `changeOwner` on the tree
    //    containing Foo, which will use a TreeTypeMap

    // 3. In TreeTypeMap#withMappedSyms we call `cls.info.decls` on every mapped
    //    class to map its symbols, one of these mapped class is Foo, but at
    //    this point we are before the ResolveSuper phase, therefore the decls
    //    scope does not contain the methods added in ResolveSuper like `<`, so
    //    we never map them and their owner is still the old symbol for Foo,
    //    this is wrong! But the compiler will not realize this until much later

    // 4. In LinkScala2ImplClasses we replace:
    //      def <(that: Int): Boolean = Foo.super[Ordered].<(that)
    //    by:
    //      def <(that: Int): Boolean = scala.math.Ordered$class#<(this, scala.Int.box(that))
    //    This is fine, except that the owner of `<` is incorrect since 3., so the `this` tree
    //    gets an incorrect symbol

    // 5. In the backend, the compiler finally realizes that something has gone very wrong:
    //    assertion failed: Trying to access the this of another class:
    //    tree.symbol = class Test$~Foo#4444,
    //    class symbol = class Test$Foo$1#6723

    byName({
      class Foo extends Ordered[Int] {
        def compare(that: Int): Int = 0
      }
    })
  }
}
