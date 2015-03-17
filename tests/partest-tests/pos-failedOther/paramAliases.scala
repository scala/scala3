// This tests that the subclass parameter is not
// translated to a field, but is forwarded to the
// superclass parameter. Right now we need to verify
// this by inspecting the output with -Xprint:super
// TODO: Make a test that does this automatically.
class Base(val x: Int)

class Sub(x: Int) extends Base(x) {
  println(x)
}

