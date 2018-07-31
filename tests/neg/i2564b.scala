class Foo private() {
  transparent def foo = new Foo // error
}
