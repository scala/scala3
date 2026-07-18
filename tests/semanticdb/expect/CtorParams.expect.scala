package ctorparams

// Constructor-parameter occurrence edge cases (parity with scalameta #4650 / #1327).
// dotc retains every primary-constructor parameter as a param-accessor field, so each
// parameter's name range carries TWO definition occurrences: the accessor `C#x.` and the
// constructor parameter `C#<init>().(x)`. References still resolve to the accessor.

trait Show/*<-ctorparams::Show#*/[A/*<-ctorparams::Show#[A]*/]:
  def show/*<-ctorparams::Show#show().*/(a/*<-ctorparams::Show#show().(a)*/: A/*->ctorparams::Show#[A]*/): String/*->scala::Predef.String#*/

// parameter used in the constructor body: accessor + constructor-parameter occurrences;
// the body reference resolves to the accessor
class UsedInCtorOnly/*<-ctorparams::UsedInCtorOnly#*/(x/*<-ctorparams::UsedInCtorOnly#x.*//*<-ctorparams::UsedInCtorOnly#`<init>`().(x)*/: Int/*->scala::Int#*/):
  println/*->scala::Predef.println(+1).*/(x/*->ctorparams::UsedInCtorOnly#x.*/)

// parameter referenced from a method: same dual occurrence at the definition site
class UsedInMethod/*<-ctorparams::UsedInMethod#*/(x/*<-ctorparams::UsedInMethod#x.*//*<-ctorparams::UsedInMethod#`<init>`().(x)*/: Int/*->scala::Int#*/):
  def get/*<-ctorparams::UsedInMethod#get().*/: Int/*->scala::Int#*/ = x/*->ctorparams::UsedInMethod#x.*/

// context bound desugars to a synthetic `given` evidence parameter. Like method evidence
// parameters (which dotc already anchors), it gets a zero-length definition occurrence at
// the bound position for both the accessor and the constructor parameter.
class WithContextBound/*<-ctorparams::WithContextBound#*/[T/*<-ctorparams::WithContextBound#[T]*/: Show/*->ctorparams::Show#*//*->ctorparams::WithContextBound#[T]*//*<-ctorparams::WithContextBound#evidence$1.*//*<-ctorparams::WithContextBound#`<init>`().(evidence$1)*/]

// explicit, source-named using parameter: gets a constructor-parameter occurrence
class WithUsing/*<-ctorparams::WithUsing#*/(using s/*<-ctorparams::WithUsing#s.*//*<-ctorparams::WithUsing#`<init>`().(s)*/: Show/*->ctorparams::Show#*/[Int/*->scala::Int#*/])

// repeated parameter: the occurrence spans the source name `xs`, not a zero-extent point
class Repeated/*<-ctorparams::Repeated#*/(xs/*<-ctorparams::Repeated#xs.*//*<-ctorparams::Repeated#`<init>`().(xs)*/: Int/*->scala::Int#*/*)
