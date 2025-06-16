//> using options -experimental
package unroll

class Unrolled(){
  var foo = ""
  def this(s: String, n: Int = 1) = {
    this()
    foo = s + n
  }
}
