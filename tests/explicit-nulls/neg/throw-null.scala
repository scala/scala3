// `throws null` is valid program in dotty but not valid with explicit null,
// since this statement will throw `NullPointerException` during runtime.
// https://stackoverflow.com/questions/17576922/why-can-i-throw-null-in-java

class Foo {
  def test1() = {
    throw null // error: the expression cannot be `Null`
  }

  def test2() = {
    val t: Throwable | Null = ???
    throw t // error: the expression cannot be `Null`
  }
}
