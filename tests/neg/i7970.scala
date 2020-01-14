object O{
  val a: Int => Int = x => x + 1
  val b: (=> Int) => Int = a        // error
  def m(i: String): String = i
  def x(f: String => String): (=>String)=>String = f // error
  def main(args: Array[String]): Unit = x(m)("X")
}