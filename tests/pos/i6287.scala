object O{
  def m() = {
    opaque type T = Int
    object T
  }
}
object A {
  {
    opaque type T = Int
    object T
    println
  }
}