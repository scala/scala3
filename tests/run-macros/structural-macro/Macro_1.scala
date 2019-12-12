import scala.quoted._

object Macro {

  case class Record(elems: (String, Any)*) extends Selectable {
    def selectDynamic(name: String): Any = elems.find(_._1 == name).get._2
  }

  inline def toHMap(s: Selectable) <: Tuple = ${ toHMapImpl('s)}

  def toHMapImpl(s: Expr[Selectable])(given qctx:QuoteContext): Expr[Tuple] = {
    import qctx.tasty.{given, _}

    val repr = s.unseal.tpe.widenTermRefExpr.dealias

    def rec(tpe: Type): List[(String, Type)] = {
      tpe match {
        case Refinement(parent, name, info: Type) => (name, info) :: rec(parent)
        case _ => Nil
      }
    }

    def tupleElem(name: String, info: Type): Expr[Any] = {
      val nameExpr = Expr(name)
      info.seal match {
        case '[$qType] =>
          Expr.ofTuple(Seq(nameExpr, '{$s.selectDynamic($nameExpr).asInstanceOf[$qType]}))
      }
    }

    // val list = println(rec(repr))

    val ret = rec(repr).reverse.map(e => tupleElem(e._1, e._2))

    Expr.ofTuple(ret)
  }

  inline def toSelectable[T](s: Tuple)<: T = ${ toSelectableImpl('s, '[T])}

  def toSelectableImpl[T](s: Expr[Tuple], tpe: Type[T])(given qctx:QuoteContext): Expr[T] = {
    import qctx.tasty.{given, _}

    val repr = s.unseal.tpe.widenTermRefExpr.dealias

    println(repr.show)
    println(repr.showExtractors)

    // new Record((res2._1._1, res2._1._2), (res2._2._1, res2._2._2)).asInstanceOf[Record {val name: String; val age: Int} ]

    def rec(tpe: Type): List[(String, Type)] = {
      tpe match {
        // todo: check number based on prefix
        case AppliedType(_, args) => args.map{
          case AppliedType(_, ConstantType(Constant(name: String)) :: (info: Type) :: Nil) => (name, info)
        }
      }
    }
    val r = rec(repr)
    println(r)

    println(tpe.unseal.symbol)
    println(TypeIdent(tpe.unseal.symbol))

    println('{new Record()}.unseal.showExtractors)

    '{ ??? }
  }
}
