class Child extends Base {
  val x = a + foo           // don't report error cross boundary
}