import scala.quoted.*

object Macro {

  trait Selectable extends scala.Selectable:
    def selectDynamic(name: String): Any

  trait SelectableRecord extends Selectable {
    transparent inline def toTuple: Tuple = ${ toTupleImpl('this)}
  }

  trait SelectableRecordCompanion[T] {
    protected def fromUntypedTuple(elems: (String, Any)*): T
    transparent inline def fromTuple[T <: Tuple](inline s: T): Any = ${ fromTupleImpl('s, '{ (x: Array[(String, Any)]) => fromUntypedTuple(x*) } ) }
  }

  private def toTupleImpl(s: Expr[Selectable])(using qctx:Quotes) : Expr[Tuple] = {
    import quotes.reflect.*

    val repr = s.asTerm.tpe.widenTermRefByName.dealias

    def rec(tpe: TypeRepr): List[(String, TypeRepr)] = {
      tpe match {
        case Refinement(parent, name, info) =>
          info match {
            case _: TypeBounds =>
              rec(parent)
            case _: MethodType | _: PolyType | _: TypeBounds | _: ByNameType =>
              report.warning(s"Ignored `$name` as a field of the record", s)
              rec(parent)
            case info: TypeRepr =>
              (name, info) :: rec(parent)
          }

        case _ => Nil
      }
    }

    def tupleElem(name: String, info: TypeRepr): Expr[Any] = {
      val nameExpr = Expr(name)
      info.asType match { case '[t] =>
          Expr.ofTupleFromSeq(Seq(nameExpr, '{ $s.selectDynamic($nameExpr).asInstanceOf[t] }))
      }
    }

    val ret = rec(repr).reverse.map(e => tupleElem(e._1, e._2))

    Expr.ofTupleFromSeq(ret)
  }

  private def fromTupleImpl[T: Type](s: Expr[Tuple], newRecord: Expr[Array[(String, Any)] => T])(using qctx:Quotes) : Expr[Any] = {
    import quotes.reflect.*

    val repr = s.asTerm.tpe.widenTermRefByName.dealias

    def isTupleCons(sym: Symbol): Boolean = sym.owner == defn.ScalaPackageClass && sym.name == "*:"

    def extractTuple(tpe: TypeRepr, seen: Set[String]): (Set[String], (String, TypeRepr)) = {
      tpe match {
        // Tuple2(S, T) where S must be a constant string type
        case AppliedType(parent, ConstantType(StringConstant(name)) :: (info: TypeRepr) :: Nil) if (parent.typeSymbol == defn.TupleClass(2)) =>
          if seen(name) then
            report.error(s"Repeated record name: $name", s)
          (seen + name, (name, info))
        case _ =>
          report.error("Tuple type was not explicit expected `(S, T)` where S is a singleton string", s)
          (seen, ("<error>", TypeRepr.of[Any]))
      }
    }
    def rec(tpe: TypeRepr, seen: Set[String]): List[(String, TypeRepr)] = {
      if tpe =:= TypeRepr.of[EmptyTuple] then Nil
      else tpe match {
        // head *: tail
        case AppliedType(parent, List(head, tail: TypeRepr)) if isTupleCons(parent.typeSymbol) =>
          val (seen2, head2) = extractTuple(head, seen)
          head2 :: rec(tail, seen2)
        // Tuple1(...), Tuple2(...), ..., Tuple22(...)
        case AppliedType(parent, args) if defn.isTupleClass(parent.typeSymbol) =>
          (args.foldLeft((seen, List.empty[(String, TypeRepr)])){ case ((seenAcc, acc), arg) =>
            val (seen3, arg2) = extractTuple(arg, seenAcc)
            (seen3, arg2 :: acc)
          })._2
        // Tuple
        case _ =>
          report.error("Tuple type must be of known size", s)
          Nil
      }
    }

    val r = rec(repr, Set.empty)

    val refinementType = r.foldLeft(TypeRepr.of[T])((acc, e) => Refinement(acc, e._1, e._2)).asType

    refinementType match { case '[t] =>
        '{ $newRecord($s.toArray.map(e => e.asInstanceOf[(String, Any)])).asInstanceOf[t] }
    }
  }
}
