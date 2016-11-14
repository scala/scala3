package foo {
  trait id {
    def bar: Int
  }
}
package foo {
  package id { // error
    class Bar
  }
}
