object Test {
  val head1 +: _ = List(1).view: @unchecked
  val _: Int = head1  // error
}
