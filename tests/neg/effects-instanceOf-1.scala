class Test {
  null.asInstanceOf[Pure] // error
  null.asInstanceOf[Impure] // error
  "".asInstanceOf[Pure] // error
  "".asInstanceOf[Impure] // error
}
