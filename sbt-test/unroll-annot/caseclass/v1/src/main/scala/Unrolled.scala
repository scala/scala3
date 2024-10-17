package unroll

case class Unrolled(s: String, n: Int = 1){
  def foo = s + n
}
