object i4371 {
  class Foo { type A = Boo#B } // error: cyclic
  class Boo { type B = Foo#A }
}
