trait A {
  class Inner
 }
trait B extends A {
  class Inner extends super.Inner // error // error
}

