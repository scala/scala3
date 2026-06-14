class Use_2 {
  static Object testA() {
    var foo = new Outer.Foo<Integer>();
    return foo.new A<Integer>();
  }

  static Object testB() {
    return new Outer.Foo.B$();
  }
}
