object Test {
  opaque def o: Int = 3 // error
  abstract object A {} // error
  sealed object A {} // error
}