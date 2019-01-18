object App {
  class Foo { type A = Boo#B } // error
  class Boo { type B = Foo#A }
}
