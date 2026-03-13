import scala.quoted.*

package jam {
    trait JamCoreDsl {
        implicit inline def defaultJamConfig: this.JamConfig =
            new JamConfig(brewRecRegex = ".*")
        class JamConfig(val brewRecRegex: String)
        inline def brew(implicit inline config: JamConfig): Unit = ${ brewImpl() }
    }
    private object internal extends JamCoreDsl
    export internal._

    def brewImpl(using q: Quotes)(): Expr[Unit] = {
        findSelf
        '{()}
    }

    private def findSelf(using q: Quotes): Unit = {
        import q.reflect.*
        def rec(s: Symbol): Option[Symbol] = s.maybeOwner match {
            case o if o.isNoSymbol => None
            case o if o.isClassDef => Option(o)
            case o => rec(o)
        }
        rec(Symbol.spliceOwner)
    }
}