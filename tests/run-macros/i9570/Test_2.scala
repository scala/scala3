object Test {

  def main(args: Array[String]): Unit = {
    import Macros.HList.*

    val hl0n = size( HCons(("1",1), HCons(("2",2), HCons(("3",3),HNil))) )
    println(s"size(?) = $hl0n")
  }
}