// The intent of this test is test that changing the order of cases doesn't affect whether
// warnings, originally reachability warnings but exhaustivity warnings too, are emitted.
// To do so we need a case that typechecks but is statically assessed to be unreachable.
// How about... a type pattern on a sealed trait that the scrutinee type doesn't extend?

sealed trait Foo

class Bar

def test1(bar: Bar) = bar match
  case _: Foo => 1 // FIXME: this is unreachable, but reverted for i13931
  case _: Bar => 2

def test2(bar: Bar) = bar match
  case _: Bar => 2
  case _: Foo => 1
