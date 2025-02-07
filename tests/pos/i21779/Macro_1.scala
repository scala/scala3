import scala.quoted.*

object Macro:
  transparent inline def tupleXxl: Tuple =
    ${tupleXxlExpr}

  def tupleXxlExpr(using Quotes) =
    import quotes.reflect.*
    Expr.ofTupleFromSeq(
      Seq(
        Expr("a"),
        Expr(2),
        Expr(3),
        Expr(4),
        Expr(5),
        Expr(6),
        Expr(7),
        Expr(8),
        Expr(9),
        Expr(10),
        Expr(11),
        Expr(12),
        Expr(13),
        Expr(14),
        Expr(15),
        Expr(16),
        Expr(17),
        Expr(18),
        Expr(19),
        Expr(20),
        Expr(21),
        Expr(22),
        Expr(23),
      )
    )

