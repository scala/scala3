
// Tests that the "JavaNull" type added to Java types is "see through" w.r.t member selections.
class Foo {
  import java.util.ArrayList
  import java.util.Iterator

  // Test that we can select through "|JavaNull" (unsoundly).
  val x3 = new ArrayList[ArrayList[ArrayList[String]]]()
  val x4: Int = x3.get(0).get(0).get(0).length()
}
