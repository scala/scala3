class Test[T] {
  type U <: T

  type Foo[T] = Array[T]

  new T                   // error: not a class type
  new T()                 // error: not a class type
  new U                   // error: not a class type
  new U()                 // error: not a class type
  new IArray[String]      // error: not a class type
  new IArray[String]()    // error: not a class type
  new IArray[String](10)  // error: not a class type // error: too mamy arguments

  new Foo[String](10)     // ok
}