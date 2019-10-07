trait That1[A]
trait That2[A, R <: That2[A, R]]

trait T[A, Self >: Null <: That1[A] with T[A, Self]] extends That2[A, Self] {
  self: Self =>

  private var next: Self = _
  def isEmpty = next eq null

  def length: Int = {
    def loop(x: Self, cnt: Int): Int = if (x.isEmpty) cnt else loop(x.next, cnt + 1)
    loop(self, 0)
  }
}
