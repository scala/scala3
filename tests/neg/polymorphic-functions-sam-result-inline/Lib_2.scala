// The SAM types change after `mk1` and `mk2` were compiled
import scala.annotation.static

trait WithLazy:
  lazy val v = 1
  def apply(x: Int): Int

trait WithStatic:
  def apply(x: Int): Int
  def get: Int = WithStatic.s
object WithStatic:
  @static val s: Int = "static".length
