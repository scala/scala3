// This is OK
object test1:
  class FunctorImpl[Generic1[T] <: Iterable[T]]{}
  class HKT3_1[Functor[Generic2[T]<:Set[T]]]{}
  var h = new HKT3_1[FunctorImpl]();

// This is has error
object test2:
  class FunctorImpl[Generic1[T] <: Iterable[T]]{}
  class HKT3_1[Functor[Generic2[T<:String]<:Set[T]]]{}
  var h = new HKT3_1[FunctorImpl]();  // error // error

