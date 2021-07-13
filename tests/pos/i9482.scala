import scala.reflect.OptManifest

object Ref {
  def make[A: OptManifest]: Ref[A] = ???
}
trait Ref[A]

trait Foo[A] {
  val bar = Ref.make[Int]
  val baz: Ref[A] = Ref.make
}
