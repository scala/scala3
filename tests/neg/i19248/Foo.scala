trait Foo {
  class Bar

  type T = Foo.this.Bar

  inline def f: Int = ???
}
