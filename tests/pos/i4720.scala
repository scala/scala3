object Test {
  def main(args: Array[String]):Unit = m(1)
  def m[Y<:Int, Z>:Int, W>:Z<:Y](d:Y):Unit={}
}
