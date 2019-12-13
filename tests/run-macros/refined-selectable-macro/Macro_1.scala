import scala.quoted._

object Macro {
  case class Record(elems: (String, Any)*) extends Selectable {
    def selectDynamic(name: String): Any = elems.find(_._1 == name).get._2
    def toTuple = {
      //todo: unnecessary transformation?
      Tuple.fromArray(elems.toArray)
    }
  }

  object Record {
    def fromTuple(t: Tuple): Record = Record(t.toArray.toSeq.map(e => e.asInstanceOf[(String, Any)]): _*)
  }

  inline def toTuple(s: Selectable) <: Tuple = ${ toTupleImpl('s)}

  def toTupleImpl(s: Expr[Selectable])(given qctx:QuoteContext): Expr[Tuple] = {
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

    val ret = rec(repr).reverse.map(e => tupleElem(e._1, e._2))

    Expr.ofTuple(ret)
  }

  // note: T is not used ATM
  inline def toSelectable[T](s: Tuple) <: Any = ${ toSelectableImpl('s, '[T])}

  def toSelectableImpl[T](s: Expr[Tuple], tpe: Type[T])(given qctx:QuoteContext): Expr[Any] = {
    import qctx.tasty.{given, _}

    val repr = s.unseal.tpe.widenTermRefExpr.dealias

    def rec(tpe: Type): List[(String, Type)] = {
      tpe match {
        // todo:
        //  check number based on prefix
        //  capture the TupleXX variants in recursion
        case AppliedType(_, args) => args.map{
          case AppliedType(_, ConstantType(Constant(name: String)) :: (info: Type) :: Nil) => (name, info)
        }
      }
    }

    val r = rec(repr)

    val refinementType = r.foldLeft('[Record].unseal.tpe)((acc, e) => Refinement(acc, e._1, e._2)).seal

    refinementType match {
      case '[$qType] =>
        '{ Record.fromTuple(${s}).asInstanceOf[${qType}] }
    }
  }
}
