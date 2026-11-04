def m[U](p: java.util.function.IntFunction[? <: U]): Unit = {}

def test(): Unit = {
  val y = "d"
  m((p) => y.asInstanceOf[Any])
}
