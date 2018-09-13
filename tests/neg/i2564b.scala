class Foo private() {
  inline def foo = new Foo // error
}
