package object foo {
  val x = 1
}
object Test {
  foo.eq(???) // error
  foo.==(???) // error
}
