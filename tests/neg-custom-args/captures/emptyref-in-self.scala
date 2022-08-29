class Zip[A, B](underlying: String, other: {*} String) {
  this: {underlying, other} Zip[A, B] =>  // error
}
