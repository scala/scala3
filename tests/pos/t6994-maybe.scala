//> using options -Yexplicit-nulls
import language.experimental.errorHandling
object Test {
  object NF {
    def unapply(t: Throwable): Throwable? = null
  }
  val x = (try { None } catch { case NF(ex) => None }) getOrElse 0
  // Was emitting a spurious warning post typer:
  // "This catches all Throwables. If this is really intended, use `case ex6 : Throwable` to clear this warning."
}
