//> using options -Ymagic-offset-header:TEST_MARKER
// Test that Symbol.sourcePos applies magic-offset-header remapping.
// DoubleDefinition errors use Symbol.srcPos, which must also remap
// through the magic header like Positioned.sourcePos does.
abstract class Wrapper {
  def foo: String  // a definition before the marker
///TEST_MARKER:tests/neg/magic-offset-header-e.scala

  def foo: Int = "not an int"  // anypos-error  // anypos-error
}
