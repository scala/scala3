package ctorparams

// Constructor-parameter occurrence edge cases (parity with scalameta #4650 / #1327).
// dotc retains every primary-constructor parameter as a param-accessor field, so each
// parameter's name range carries TWO definition occurrences: the accessor `C#x.` and the
// constructor parameter `C#<init>().(x)`. References still resolve to the accessor.

trait Show[A]:
  def show(a: A): String

// parameter used in the constructor body: accessor + constructor-parameter occurrences;
// the body reference resolves to the accessor
class UsedInCtorOnly(x: Int):
  println(x)

// parameter referenced from a method: same dual occurrence at the definition site
class UsedInMethod(x: Int):
  def get: Int = x

// context bound desugars to a synthetic `given` evidence parameter. Like method evidence
// parameters (which dotc already anchors), it gets a zero-length definition occurrence at
// the bound position for both the accessor and the constructor parameter.
class WithContextBound[T: Show]

// explicit, source-named using parameter: gets a constructor-parameter occurrence
class WithUsing(using s: Show[Int])

// repeated parameter: the occurrence spans the source name `xs`, not a zero-extent point
class Repeated(xs: Int*)
