
// Tests that we add "|JavaNull" to Java fields and methods from classfiles.
class Foo {

  def foo = {
    import java.util.ArrayList
    import java.util.Iterator
    val x = new ArrayList[String]() 
    x.add(null) // |JavaNull added to method argument
    // TODO(abeln): we can't directly test that |JavaNull was added to the return type 
    // in a positive test. We test it in the negative counterpart instead.

    // Test that we can select through "|JavaNull" (unsoundly).
    val x3 = new ArrayList[ArrayList[ArrayList[String]]]()
    val x4: Int = x3.get(0).get(0).get(0).length()
  }
}
