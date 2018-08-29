
// Tests that we add "| JavaNull" to Java fields and methods from classfiles.
class Foo {

  def foo = {
    import java.util.ArrayList
    import java.util.Iterator
    val x = new ArrayList[String]() 
    x.add(null) // | JavaNull added to a method argument

    // Test that we can select through "|JavaNull" (unsoundly).
    val x2 = new ArrayList[ArrayList[ArrayList[String]]]()
    val x3 = x2.get(0).get(0)
  }
}
