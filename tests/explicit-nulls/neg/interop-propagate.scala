 class Foo {
  import java.util.ArrayList

  // Test that as we extract return values, we're missing the |UncheckedNull in the return type.
  // i.e. test that the nullability is propagated to nested containers.
  val ll = new ArrayList[ArrayList[ArrayList[String]]]
  val level1: ArrayList[ArrayList[String]] = ll.get(0) // error
  val level2: ArrayList[String] = ll.get(0).get(0) // error
  val level3: String = ll.get(0).get(0).get(0) // error
  val ok: String = ll.get(0).get(0).get(0) // error
}
