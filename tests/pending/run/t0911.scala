class Foo(val bar : () => String);

class IP extends Foo(() => baz) {
// TODO NEEDS MANUAL CHANGE (early initializers)
// BEGIN copied early initializers
val baz = "bar"
// END copied early initializers
};

object Test extends dotty.runtime.LegacyApp{
  (new IP).bar();
}
