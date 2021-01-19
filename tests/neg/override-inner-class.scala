class C with
  type T >: String <: Any

class D extends C with
  class T  // error
