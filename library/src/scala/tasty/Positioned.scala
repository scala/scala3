package scala.tasty

trait Positioned {
  def pos(implicit ctx: Context): Position
}
