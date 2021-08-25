package example

class MyClass

class MembersContainer {
  type MyType[T <: MyClass] = Comparable[T]
}

object Exports {
  val instance = new MembersContainer
  export instance.*
}
