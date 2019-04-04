// Check that `throws null` still typechecks.
// https://stackoverflow.com/questions/17576922/why-can-i-throw-null-in-java

class Foo {
  throw null // throws an npe
  val npe: NullPointerException|Null = ???
  throw npe
}
