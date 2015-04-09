object nameddefaults {

  def foo(first: Int, second: Int = 2, third: Int = 3) = first + second

  var x = 1
  var y = 2

  foo(1, 2, 3)

  foo(1, 2)

  foo(1)

  // named and missing arguments

  foo(first = 1, second = 3)

  foo(second = 3, first = 1)

  foo(first = 2, third = 3)

  foo(2, third = 3)

  // same but with non-idempotent expressions

  foo(first = x, second = y)

  foo(second = x, first = y)

  foo(first = x, third = y)

  foo(x, third = y)

// The same thing, but for classes

  class C(first: Int, second: Int = 2, third: Int = 3) {}

  new C(1, 2, 3)

  new C(1, 2)

  new C(1)

  // named and missing arguments

  new C(first = 1, second = 3)

  new C(second = 3, first = 1)

  new C(first = 2, third = 3)

  new C(2, third = 3)

  // same but with non-idempotent expressions

  new C(first = x, second = y)

  new C(second = x, first = y)

  new C(first = x, third = y)


}
