object App {
  class Foo { type A = Boo#B } // error: illegal cyclic reference: alias App.Boo#B of type A refers back to the type itself
  class Boo { type B = Foo#A }
}
