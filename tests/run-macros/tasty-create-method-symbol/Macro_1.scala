import quoted._

object Macros {

  inline def theTestBlock : Unit = ${ theTestBlockImpl }

  def theTestBlockImpl(using q: Quotes) : Expr[Unit] = {
    import quotes.reflect._

    // simple smoke test
    val sym1 : Symbol = Symbol.newMethod(
      Symbol.spliceOwner,
      "sym1",
      MethodType(List("a","b"))(
        _ => List(TypeRepr.of[Int], TypeRepr.of[Int]),
        _ => TypeRepr.of[Int]))
    assert(sym1.isDefDef)
    assert(sym1.name == "sym1")
    val sym1Statements : List[Statement] = List(
      DefDef(sym1, {
        case List() => {
          case List(List(a, b)) =>
            Some(Term.of('{ ${ a.asExpr.asInstanceOf[Expr[Int]] } - ${ b.asExpr.asInstanceOf[Expr[Int]] } }))
        }
      }),
      Term.of('{ assert(${ Apply(Ref(sym1), List(Literal(Constant.Int(2)), Literal(Constant.Int(3)))).asExpr.asInstanceOf[Expr[Int]] } == -1) }))

    // test for no argument list (no Apply node)
    val sym2 : Symbol = Symbol.newMethod(
      Symbol.spliceOwner,
      "sym2",
      ByNameType(TypeRepr.of[Int]))
    assert(sym2.isDefDef)
    assert(sym2.name == "sym2")
    val sym2Statements : List[Statement] = List(
      DefDef(sym2, {
        case List() => {
          case List() =>
            Some(Literal(Constant.Int(2)))
        }
      }),
      Term.of('{ assert(${ Ref(sym2).asExpr.asInstanceOf[Expr[Int]] } == 2) }))

   // test for multiple argument lists
   val sym3 : Symbol = Symbol.newMethod(
      Symbol.spliceOwner,
      "sym3",
      MethodType(List("a"))(
        _ => List(TypeRepr.of[Int]),
        mt => MethodType(List("b"))(
          _ => List(mt.param(0)),
          _ => mt.param(0))))
    assert(sym3.isDefDef)
    assert(sym3.name == "sym3")
    val sym3Statements : List[Statement] = List(
      DefDef(sym3, {
        case List() => {
          case List(List(a), List(b)) =>
            Some(a)
        }
      }),
      Term.of('{ assert(${ Apply(Apply(Ref(sym3), List(Literal(Constant.Int(3)))), List(Literal(Constant.Int(3)))).asExpr.asInstanceOf[Expr[Int]] } == 3) }))

    // test for recursive references
    val sym4 : Symbol = Symbol.newMethod(
      Symbol.spliceOwner,
      "sym4",
      MethodType(List("x"))(
        _ => List(TypeRepr.of[Int]),
        _ => TypeRepr.of[Int]))
    assert(sym4.isDefDef)
    assert(sym4.name == "sym4")
    val sym4Statements : List[Statement] = List(
      DefDef(sym4, {
        case List() => {
          case List(List(x)) =>
            Some(Term.of('{
              if ${ x.asExpr.asInstanceOf[Expr[Int]] } == 0
              then 0
              else ${ Apply(Ref(sym4), List(Term.of('{ ${ x.asExpr.asInstanceOf[Expr[Int]] } - 1 }))).asExpr.asInstanceOf[Expr[Int]] }
            }))
        }
      }),
      Term.of('{ assert(${ Apply(Ref(sym4), List(Literal(Constant.Int(4)))).asExpr.asInstanceOf[Expr[Int]] } == 0) }))

    // test for nested functions (one symbol is the other's parent, and we use a Closure)
    val sym5 : Symbol = Symbol.newMethod(
      Symbol.spliceOwner,
      "sym5",
      MethodType(List("x"))(
        _ => List(TypeRepr.of[Int]),
        _ => TypeRepr.of[Int=>Int]))
    assert(sym5.isDefDef)
    assert(sym5.name == "sym5")
    val sym5Statements : List[Statement] = List(
      DefDef(sym5, {
        case List() => {
          case List(List(x)) =>
            Some {
              val sym51 : Symbol = Symbol.newMethod(
                sym5,
                "sym51",
                MethodType(List("x"))(
                  _ => List(TypeRepr.of[Int]),
                  _ => TypeRepr.of[Int]))
              Block(
                List(
                  DefDef(sym51, {
                    case List() => {
                      case List(List(xx)) =>
                        Some(Term.of('{ ${ x.asExpr.asInstanceOf[Expr[Int]] } - ${ xx.asExpr.asInstanceOf[Expr[Int]] } }))
                    }
                  })),
                Closure(Ref(sym51), None))
            }
        }
      }),
      Term.of('{ assert(${ Apply(Ref(sym5), List(Literal(Constant.Int(5)))).asExpr.asInstanceOf[Expr[Int=>Int]] }(4) == 1) }))

    // test mutually recursive definitions
    val sym6_1 : Symbol = Symbol.newMethod(
      Symbol.spliceOwner,
      "sym6_1",
      MethodType(List("x"))(
        _ => List(TypeRepr.of[Int]),
        _ => TypeRepr.of[Int]))
    val sym6_2 : Symbol = Symbol.newMethod(
      Symbol.spliceOwner,
      "sym6_2",
      MethodType(List("x"))(
        _ => List(TypeRepr.of[Int]),
        _ => TypeRepr.of[Int]))
    assert(sym6_1.isDefDef)
    assert(sym6_2.isDefDef)
    assert(sym6_1.name == "sym6_1")
    assert(sym6_2.name == "sym6_2")
    val sym6Statements : List[Statement] = List(
      DefDef(sym6_1, {
        case List() => {
          case List(List(x)) =>
            Some {
              Term.of('{
                println(s"sym6_1: ${ ${ x.asExpr.asInstanceOf[Expr[Int]] } }")
                if ${ x.asExpr.asInstanceOf[Expr[Int]] } == 0
                then 0
                else ${ Apply(Ref(sym6_2), List(Term.of('{ ${ x.asExpr.asInstanceOf[Expr[Int]] } - 1 }))).asExpr.asInstanceOf[Expr[Int]] }
              })
            }
        }
      }),
      DefDef(sym6_2, {
        case List() => {
          case List(List(x)) =>
            Some {
              Term.of('{
                println(s"sym6_2: ${ ${ x.asExpr.asInstanceOf[Expr[Int]] } }")
                if ${ x.asExpr.asInstanceOf[Expr[Int]] } == 0
                then 0
                else ${ Apply(Ref(sym6_1), List(Term.of('{ ${ x.asExpr.asInstanceOf[Expr[Int]] } - 1 }))).asExpr.asInstanceOf[Expr[Int]] }
              })
            }
        }

      }),
      Term.of('{ assert(${ Apply(Ref(sym6_2), List(Literal(Constant.Int(6)))).asExpr.asInstanceOf[Expr[Int]] } == 0) }))

    // test polymorphic methods by synthesizing an identity method
    val sym7 : Symbol = Symbol.newMethod(
      Symbol.spliceOwner,
      "sym7",
      PolyType(List("T"))(
        tp => List(TypeBounds(TypeRepr.of[Nothing], TypeRepr.of[Any])),
        tp => MethodType(List("t"))(
          _ => List(tp.param(0)),
          _ => tp.param(0))))
    assert(sym7.isDefDef)
    assert(sym7.name == "sym7")
    val sym7Statements : List[Statement] = List(
      DefDef(sym7, {
        case List(t) => {
          case List(List(x)) =>
            Some(Typed(x, Inferred(t)))
        }
      }),
      Term.of('{ assert(${ Apply(TypeApply(Ref(sym7), List(Inferred(TypeRepr.of[Int]))), List(Literal(Constant.Int(7)))).asExpr.asInstanceOf[Expr[Int]] } == 7) }))

    Block(
      sym1Statements ++
      sym2Statements ++
      sym3Statements ++
      sym4Statements ++
      sym5Statements ++
      sym6Statements ++
      sym7Statements ++
      List(Term.of('{ println("Ok") })),
      Literal(Constant.Unit())).asExpr.asInstanceOf[Expr[Unit]]
  }
}

