
object Foo {
  new Function0
  new Function22
  new Function30
  new Function31 // error: not found: type Function31
  new Function100000 // error: not found: type Function100000
  new `Function-1` // error : not found: type Function-1
}
