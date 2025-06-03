

case class Token(tokenType: TokenType, lexeme: StringV, line: IntV)

sealed trait TokenType
  // Single-character tokens.
object MINUS extends TokenType
object PLUS extends TokenType
object SLASH extends TokenType
object STAR extends TokenType
object BANG extends TokenType
object BANG_EQUAL extends TokenType
object EQUAL extends TokenType
object EQUAL_EQUAL extends TokenType
object GREATER extends TokenType
object GREATER_EQUAL extends TokenType
object LESS extends TokenType
object LESS_EQUAL extends TokenType

sealed trait Expr
case class Binary(left: Expr, operator: Token, right: Expr) extends Expr

sealed trait Value
case class StringV(v: String) extends Value
case class IntV(v: Int) extends Value
case class DoubleV(v: Double) extends Value
case class BooleanV(v: Boolean) extends Value
case class UnitV() extends Value

object Interpreter {
    def eval(expr: Expr): Value =
        expr match {
            case Binary(left, op, right) =>
                val l = eval(left)
                val r = eval(right)
                (l, r, op.tokenType) match {    // warn
                    case (l: DoubleV, r: DoubleV, PLUS)          => ???
                    case (l: IntV, r: IntV, PLUS)                => ???
                    case (l: DoubleV, r: IntV, PLUS)             => ???
                    case (l: IntV, r: DoubleV, PLUS)             => ???
                    case (l: StringV, r: StringV, PLUS)          => ???
                    case (l: DoubleV, r: DoubleV, MINUS)         => ???
                    case (l: IntV, r: IntV, MINUS)               => ???
                    case (l: DoubleV, r: IntV, MINUS)            => ???
                    case (l: IntV, r: DoubleV, MINUS)            => ???
                    case (l: DoubleV, r: DoubleV, STAR)          => ???
                    case (l: IntV, r: IntV, STAR)                => ???
                    case (l: DoubleV, r: IntV, STAR)             => ???
                    case (l: IntV, r: DoubleV, STAR)             => ???
                    case (l: DoubleV, r: DoubleV, SLASH)         => ???
                    case (l: IntV, r: IntV, SLASH)               => ???
                    case (l: DoubleV, r: IntV, SLASH)            => ???
                    case (l: IntV, r: DoubleV, SLASH)            => ???
                    case (l: DoubleV, r: DoubleV, GREATER)       => ???
                    case (l: IntV, r: IntV, GREATER)             => ???
                    case (l: DoubleV, r: IntV, GREATER)          => ???
                    case (l: IntV, r: DoubleV, GREATER)          => ???
                    case (l: StringV, r: StringV, GREATER)       => ???
                    case (l: DoubleV, r: DoubleV, LESS)          => ???
                    case (l: IntV, r: IntV, LESS)                => ???
                    case (l: DoubleV, r: IntV, LESS)             => ???
                    case (l: IntV, r: DoubleV, LESS)             => ???
                    case (l: StringV, r: StringV, LESS)          => ???
                    case (l: DoubleV, r: DoubleV, LESS_EQUAL)    => ???
                    case (l: IntV, r: IntV, LESS_EQUAL)          => ???
                    case (l: DoubleV, r: IntV, LESS_EQUAL)       => ???
                    case (l: IntV, r: DoubleV, LESS_EQUAL)       => ???
                    case (l: StringV, r: StringV, LESS_EQUAL)    => ???
                    case (l: DoubleV, r: DoubleV, GREATER_EQUAL) => ???
                    case (l: IntV, r: IntV, GREATER_EQUAL)       => ???
                    case (l: DoubleV, r: IntV, GREATER_EQUAL)    => ???
                    case (l: IntV, r: DoubleV, GREATER_EQUAL)    => ???
                    case (l: StringV, r: StringV, GREATER_EQUAL) => ???
                    // case _                                       => ???
                }
        }

}
