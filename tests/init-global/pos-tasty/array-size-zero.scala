object A:
  val emptyArray = new Array(0)

object B:
  def build(data: Int*) =
    if data.size == 0 then A.emptyArray else Array(data)

  val arr = build(5, 6)
  val first = arr(0)

