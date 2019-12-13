import scala.quoted._

object Macro {

  trait SelectableRecord extends Selectable {
    inline def toTuple <: Tuple = ${ toTupleImpl('this)}
  }

  trait SelectableRecordCompanion[T] {
    protected def fromUntypedTuple(elems: (String, Any)*): T
    inline def fromTuple[T <: Tuple](s: T) <: Any = ${ fromTupleImpl('s, '{ (x: Array[(String, Any)]) => fromUntypedTuple(x: _*) } ) }
  }

  private def toTupleImpl(s: Expr[Selectable])(given qctx:QuoteContext): Expr[Tuple] = {
    import qctx.tasty.{given, _}

    val repr = s.unseal.tpe.widenTermRefExpr.dealias

    def rec(tpe: Type): List[(String, Type)] = {
      tpe match {
        case Refinement(parent, name, info) =>
          info match {
            case _: TypeBounds =>
              rec(parent)
            case _: MethodType | _: PolyType | _: TypeBounds =>
              qctx.warning(s"Ignored $name as a field of the record", s)
              rec(parent)
            case info: Type =>
              (name, info) :: rec(parent)
          }

        case _ => Nil
      }
    }

    def tupleElem(name: String, info: Type): Expr[Any] = {
      val nameExpr = Expr(name)
      info.seal match { case '[$qType] =>
          Expr.ofTuple(Seq(nameExpr, '{ $s.selectDynamic($nameExpr).asInstanceOf[$qType] }))
      }
    }

    val ret = rec(repr).reverse.map(e => tupleElem(e._1, e._2))

    Expr.ofTuple(ret)
  }

  private def fromTupleImpl[T: Type](s: Expr[Tuple], newRecord: Expr[Array[(String, Any)] => T])(given qctx:QuoteContext): Expr[Any] = {
    import qctx.tasty.{given, _}

    val repr = s.unseal.tpe.widenTermRefExpr.dealias

    def isTupleCons(sym: Symbol): Boolean = sym.owner == defn.ScalaPackageClass && sym.name == "*:"

    def extractTuple(tpe: TypeOrBounds, seen: Set[String]): (Set[String], (String, Type)) = {
      tpe match {
        // Tuple2(S, T) where S must be a constant string type
        case AppliedType(parent, ConstantType(Constant(name: String)) :: (info: Type) :: Nil) if (parent.typeSymbol == defn.TupleClass(2)) =>
          if seen(name) then
            qctx.error(s"Repeated record name: $name", s)
          (seen + name, (name, info))
        case _ =>
          qctx.error("Tuple type was not explicit expected `(S, T)` where S is a singleton string", s)
          (seen, ("<error>", defn.AnyType))
      }
    }
    def rec(tpe: Type, seen: Set[String]): List[(String, Type)] = {
      if tpe =:= defn.UnitType then Nil
      else tpe match {
        // head *: tail
        case AppliedType(parent, List(head, tail: Type)) if isTupleCons(parent.typeSymbol) =>
          val (seen2, head2) = extractTuple(head, seen)
          head2 :: rec(tail, seen2)
        // Tuple1(...), Tuple2(...), ..., Tuple22(...)
        case AppliedType(parent, args) if defn.isTupleClass(parent.typeSymbol) =>
          (args.foldLeft((seen, List.empty[(String, Type)])){ case ((seenAcc, acc), arg) =>
            val (seen3, arg2) = extractTuple(arg, seenAcc)
            (seen3, arg2 :: acc)
          })._2
        // Tuple
        case _ =>
          qctx.error("Tuple type must be of known size", s)
          Nil
      }
    }

    val r = rec(repr, Set.empty)

    val refinementType = r.foldLeft('[T].unseal.tpe)((acc, e) => Refinement(acc, e._1, e._2)).seal

    refinementType match { case '[$qType] =>
        '{ $newRecord($s.toArray.map(e => e.asInstanceOf[(String, Any)])).asInstanceOf[${qType}] }
    }
  }
}
