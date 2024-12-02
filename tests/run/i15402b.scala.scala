trait Adapter[T] extends Function1[T, Unit]

// Works in Scala 2.12 and 2.13 but generates wrong bytecode for Scala 3
// due to using `(arg: Number) => ()` instead of `(arg: T) => ()`
def makeAdapter[T <: Number]: Adapter[T] = (arg: Number) => ()

// In Scala 3 this caused a java.lang.AbstractMethodError
@main def Test = makeAdapter[Integer](123)

