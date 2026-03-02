object types {
  final case class MyClass1(
    int: Int,
    string: String,
    boolean: Boolean,
  ) {
    final val finalVal: String = "result"
  }
}
