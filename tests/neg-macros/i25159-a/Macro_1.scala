package first
import scala.quoted.{Quotes, Expr}

object Static :

  private def callInfoImpl()(using q: Quotes): Expr[String] =
    import q.reflect.{Symbol, Ref}

    val clsSym = Symbol.spliceOwner.owner.owner

    def getArgExpr(symbols: List[Symbol]):  Expr[List[String]] =
      def extract(symbol: Symbol) =
        val symbolRef = Expr(symbol.name + "=")
        val symbolVal = Ref(symbol).asExprOf[Any]
        '{ $symbolRef + $symbolVal.toString }
      val args = symbols.map(extract)
      Expr.ofList(args)

    def classArguments(): Expr[List[String]] =
      getArgExpr(clsSym.primaryConstructor.paramSymss.flatten)

    def makeClassLine(className: String, arguments: Expr[List[String]]): Expr[String] =
      '{ ${Expr(className)} + "(" + $arguments.mkString(",") + ")"}

    makeClassLine(clsSym.fullName,classArguments())

  inline def callInfo(): String = ${ callInfoImpl() }
