case class Token(tokenType: TokenType, lexeme: String, line: Int)

enum TokenType:
  // Single-character tokens.
  case MINUS, PLUS, SLASH, STAR,

  // One or two character tokens.
  BANG, BANG_EQUAL,
  EQUAL, EQUAL_EQUAL,
  GREATER, GREATER_EQUAL,
  LESS, LESS_EQUAL

enum Expr {
    case Binary(left: Expr, operator: Token, right: Expr)
}

object Interpreter:
    import Expr.*
    import TokenType.*

    def eval(expr: Expr): String | Int | Double | Boolean | Unit =
        expr match
            case Binary(left, op, right) =>
                val l = eval(left)
                val r = eval(right)
                (l, r, op.tokenType) match
                    case (l: Double, r: Double, PLUS) => l + r
                    case (l: Int, r: Int, PLUS) => l + r
                    case (l: Double, r: Int, PLUS) => l + r
                    case (l: Int, r: Double, PLUS) => l + r
                    case (l: String, r: String, PLUS) => l + r
                    case (l: Double, r: Double, MINUS) => l - r
                    case (l: Int, r: Int, MINUS) => l - r
                    case (l: Double, r: Int, MINUS) => l - r
                    case (l: Int, r: Double, MINUS) => l - r
                    case (l: Double, r: Double, STAR) => l * r
                    case (l: Int, r: Int, STAR) => l * r
                    case (l: Double, r: Int, STAR) => l * r
                    case (l: Int, r: Double, STAR) => l * r
                    case (l: Double, r: Double, SLASH) => l / r
                    case (l: Int, r: Int, SLASH) => l / r
                    case (l: Double, r: Int, SLASH) => l / r
                    case (l: Int, r: Double, SLASH) => l / r
                    case (l: Double, r: Double, GREATER) => l > r
                    case (l: Int, r: Int, GREATER) => l > r
                    case (l: Double, r: Int, GREATER) => l > r
                    case (l: Int, r: Double, GREATER) => l > r
                    case (l: String, r: String, GREATER) => l > r
                    case (l: Double, r: Double, LESS) => l < r
                    case (l: Int, r: Int, LESS) => l < r
                    case (l: Double, r: Int, LESS) => l < r
                    case (l: Int, r: Double, LESS) => l < r
                    case (l: String, r: String, LESS) => l < r
                    case (l: Double, r: Double, LESS_EQUAL) => l <= r
                    case (l: Int, r: Int, LESS_EQUAL) => l <= r
                    case (l: Double, r: Int, LESS_EQUAL) => l <= r
                    case (l: Int, r: Double, LESS_EQUAL) => l <= r
                    case (l: String, r: String, LESS_EQUAL) => l <= r
                    case (l: Double, r: Double, GREATER_EQUAL) => l >= r
                    case (l: Int, r: Int, GREATER_EQUAL) => l >= r
                    case (l: Double, r: Int, GREATER_EQUAL) => l >= r
                    case (l: Int, r: Double, GREATER_EQUAL) => l >= r
                    case (l: String, r: String, GREATER_EQUAL) => l >= r
                    case _ => println("unsupported operation")
