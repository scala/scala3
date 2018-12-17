trait SAM {
  type T >: Int
  def apply(x: Int): Int
  def t: T = 1
}

object O{
  def main(a:Array[String])={
    val fn: SAM {type T = String} = (i:Int) => i  // error: SAM{T = String} is not a legal SAM type
    def cce: String = fn.t
    println(cce)
  }
}