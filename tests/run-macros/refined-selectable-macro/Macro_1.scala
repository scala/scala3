import scala.quoted._

object Macro {

  trait Selectable extends scala.Selectable:
    def selectDynamic(name: String): Any

  trait SelectableRecord extends Selectable {
    transparent inline def toTuple: Tuple = ${ toTupleImpl('this)}
  }

  trait SelectableRecordCompanion[T] {
    protected def fromUntypedTuple(elems: (String, Any)*): T
    transparent inline def fromTuple[T <: Tuple](inline s: T): Any = ${ fromTupleImpl('s, '{ (x: Array[(String, Any)]) => fromUntypedTuple(x: _*) } ) }
  }

  private def toTupleImpl(using s: Scope)(e: s.Expr[Selectable]): s.Expr[Tuple] = {
    import s.tasty._

    val repr = e.tpe.widenTermRefExpr.dealias

    def rec(tpe: Type): List[(String, Type)] = {
      tpe match {
        case Refinement(parent, name, info) =>
          info match {
            case _: TypeBounds =>
              rec(parent)
            case _: MethodType | _: PolyType | _: TypeBounds | _: ByNameType =>
              report.warningOn(e, s"Ignored `$name` as a field of the record")
              rec(parent)
            case info: Type =>
              (name, info) :: rec(parent)
          }

        case _ => Nil
      }
    }

    def tupleElem(name: String, info: Type): s.Expr[Any] = {
      val nameExpr = Expr(name)
      info.seal.get match { case '[$qType] =>
          Expr.ofTupleFromSeq(Seq(nameExpr, '{ $e.selectDynamic($nameExpr).asInstanceOf[$qType] }))
      }
    }

    val ret = rec(repr).reverse.map(e => tupleElem(e._1, e._2))

    Expr.ofTupleFromSeq(ret)
  }

  private def fromTupleImpl[T](using s: Scope)(e: s.Expr[Tuple], newRecord: s.Expr[Array[(String, Any)] => T])(using s.Type[T]): s.Expr[Any] = {
    import s.tasty._

    val repr = e.tpe.widenTermRefExpr.dealias

    def isTupleCons(sym: Symbol): Boolean = sym.owner == defn.ScalaPackageClass && sym.name == "*:"

    def extractTuple(tpe: Type, seen: Set[String]): (Set[String], (String, Type)) = {
      tpe match {
        // Tuple2(S, T) where S must be a constant string type
        case AppliedType(parent, ConstantType(Constant(name: String)) :: (info: Type) :: Nil) if (parent.typeSymbol == defn.TupleClass(2)) =>
          if seen(name) then
            report.errorOn(e, s"Repeated record name: $name")
          (seen + name, (name, info))
        case _ =>
          report.errorOn(e, "Tuple type was not explicit expected `(S, T)` where S is a singleton string")
          (seen, ("<error>", Type.of[Any]))
      }
    }
    def rec(tpe: Type, seen: Set[String]): List[(String, Type)] = {
      if tpe =:= Type.of[EmptyTuple] then Nil
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
          report.errorOn(e, "Tuple type must be of known size")
          Nil
      }
    }

    val r = rec(repr, Set.empty)

    val refinementType = r.foldLeft('[T].tpe)((acc, e) => Refinement(acc, e._1, e._2)).seal.get

    refinementType match { case '[$qType] =>
        '{ $newRecord($e.toArray.map(e => e.asInstanceOf[(String, Any)])).asInstanceOf[${qType}] }
    }
  }
}
