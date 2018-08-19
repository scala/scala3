class Foo private() {
  rewrite def foo = new Foo // error
}
