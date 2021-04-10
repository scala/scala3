class Test[T] {
  type U <: T

  type Foo[T] = Array[T]

  new T                   // error: does not have a constructor
  new T()                 // error: does not have a constructor
  new U                   // error: does not have a constructor
  new U()                 // error: does not have a constructor
  new IArray[String]      // error: does not have a constructor
  new IArray[String]()    // error: does not have a constructor
  new IArray[String](10)  // error: does not have a constructor

  new Foo[String](10)     // ok
}