// minimized facade
object Object {
  def assign[T, U](target: T, source: U): T with U = js.native
}