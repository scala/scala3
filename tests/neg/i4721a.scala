object Test {
  def main(args: Array[String]):Unit = m("1") // error
  def m[Y<:String, Z>:Int, W>:Z<:Y](d:Y):Unit={}
}
