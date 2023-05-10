class Zip[A, B](underlying: String, other: String^) {
  this: Zip[A, B]^{underlying, other} =>  // error
}
