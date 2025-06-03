// An example of how constructor _type_ parameters
// Which can _not_ be passed to the extends part
// That makes it part of the parent type,
// which has been found to be unsound.
class Foo[A]
class Foo1(val x: Int)
  extends Foo[ // error: The type of a class parent cannot refer to constructor parameters, but Foo[(Foo1.this.x : Int)] refers to x
    x.type
  ]
