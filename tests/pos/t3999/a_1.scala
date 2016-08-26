package foo

class Outside

package object bar {
  class Val(b: Boolean)
  implicit def boolean2Val(b: Boolean): foo.bar.`package`.Val = new Val(b)
  implicit def boolean2Outside(b: Boolean): foo.Outside = new Outside
}
