package a

object A {

  class Buf[A] {
    def append(a: A): this.type = this
    def append(a: A*): this.type = this
  }

}
