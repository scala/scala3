// ReproMacro.scala
// Minimal macro that reproduces "Coll[Int] is not a legal path" error
// when creating new ValDefs with types containing @retains annotations

import scala.quoted.*

object ReproMacro:

  transparent inline def transform[A](inline expr: A): A = ${ transformImpl[A]('expr) }

  private def transformImpl[A: Type](expr: Expr[A])(using Quotes): Expr[A] =
    import quotes.reflect.*

    val term = expr.asTerm
    val owner = Symbol.spliceOwner

    // Rebuild the block, creating NEW ValDefs with the same types
    val result = rebuildBlock(term, owner)

    result.asExprOf[A]

  private def rebuildBlock(using Quotes)(term: quotes.reflect.Term, owner: quotes.reflect.Symbol): quotes.reflect.Term =
    import quotes.reflect.*

    term match
      case Block(stats, expr) =>
        var symbolMap = Map.empty[Symbol, Symbol]

        val newStats = stats.map {
          case vd @ ValDef(name, tpt, Some(rhs)) =>
            // Create a new symbol with the same type - this causes the error
            // when tpt.tpe contains AnnotatedType(LazyRef(Coll[Int]), @retains(...))
            val newSym = Symbol.newVal(owner, name, tpt.tpe, Flags.EmptyFlags, Symbol.noSymbol)
            symbolMap = symbolMap + (vd.symbol -> newSym)
            ValDef(newSym, Some(substituteRefs(rhs, symbolMap)))
          case other => other
        }

        Block(newStats, substituteRefs(expr, symbolMap))

      case Inlined(call, bindings, expansion) =>
        Inlined(call, bindings, rebuildBlock(expansion, owner))

      case _ => term

  private def substituteRefs(using Quotes)(term: quotes.reflect.Term, map: Map[quotes.reflect.Symbol, quotes.reflect.Symbol]): quotes.reflect.Term =
    import quotes.reflect.*

    val mapper = new TreeMap:
      override def transformTerm(t: Term)(owner: Symbol): Term = t match
        case Ident(name) if map.contains(t.symbol) =>
          Ref(map(t.symbol))
        case _ => super.transformTerm(t)(owner)

    mapper.transformTerm(term)(Symbol.spliceOwner)
