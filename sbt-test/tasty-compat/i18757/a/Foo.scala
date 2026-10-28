object Foo {
  @scala.annotation.nowarn("msg=unused implicit parameter")
  def foo(i: Int)(implicit ev: String): Int = i + 1
}
