import scala.quoted.*
import java.lang.ref.WeakReference

// A reused staging compiler must not keep the local symbols of its past runs
object Test {

  given staging.Compiler = staging.Compiler.make(this.getClass.getClassLoader)

  def main(args: Array[String]): Unit =
    val runs = 20
    val locals = Array.fill[WeakReference[AnyRef]](runs)(null)
    for i <- 0 until runs do
      staging.run {
        import quotes.reflect.*
        val code = '{ val x = ${ Expr(i % 2 == 0) }; () => if x then 1 else 2 }
        code.asTerm.underlyingArgument match
          case Block(List(vd: ValDef), _) => locals(i) = WeakReference(vd.symbol)
        '{ $code() }
      }
    for _ <- 1 to 5 do System.gc()
    val alive = locals.count(_.get != null)
    // the latest runs may stay cached, the older ones must be collectable
    assert(alive < runs / 2, s"$alive of $runs local symbols of past runs are still reachable")
}
