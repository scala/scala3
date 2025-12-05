package interpreter

/**
 * Platform-independent AST representation for the browser interpreter.
 *
 * This AST can be serialized to/from JSON and interpreted without
 * any JVM or compiler dependencies.
 */
sealed trait Ast

object Ast {
  // Literals
  case class IntLit(value: Int) extends Ast
  case class LongLit(value: Long) extends Ast
  case class DoubleLit(value: Double) extends Ast
  case class FloatLit(value: Float) extends Ast
  case class BoolLit(value: Boolean) extends Ast
  case class StringLit(value: String) extends Ast
  case class CharLit(value: Char) extends Ast
  case object UnitLit extends Ast
  case object NullLit extends Ast

  // References
  case class Ident(name: String) extends Ast
  case class Select(receiver: Ast, name: String) extends Ast

  // Definitions
  case class ValDef(name: String, rhs: Ast, mutable: Boolean = false) extends Ast
  case class DefDef(name: String, params: List[String], body: Ast) extends Ast

  // Control flow
  case class Block(stats: List[Ast], expr: Ast) extends Ast
  case class If(cond: Ast, thenp: Ast, elsep: Ast) extends Ast
  case class While(cond: Ast, body: Ast) extends Ast
  case class Match(selector: Ast, cases: List[CaseDef]) extends Ast
  case class CaseDef(pattern: Pattern, guard: Option[Ast], body: Ast)
  case class Try(block: Ast, catches: List[CaseDef], finalizer: Option[Ast]) extends Ast
  case class Return(expr: Ast) extends Ast
  case class Throw(expr: Ast) extends Ast

  // Operations
  case class BinaryOp(op: String, lhs: Ast, rhs: Ast) extends Ast
  case class UnaryOp(op: String, arg: Ast) extends Ast
  case class Apply(fn: Ast, args: List[Ast]) extends Ast
  case class New(className: String, args: List[Ast]) extends Ast
  case class Assign(name: String, rhs: Ast) extends Ast

  // Functions
  case class Lambda(params: List[String], body: Ast) extends Ast

  // Patterns
  sealed trait Pattern
  object Pattern {
    case object Wildcard extends Pattern
    case class Bind(name: String, inner: Option[Pattern] = None) extends Pattern
    case class Literal(value: Any) extends Pattern
    case class Typed(tpe: String, inner: Option[Pattern] = None) extends Pattern
    case class Unapply(className: String, patterns: List[Pattern]) extends Pattern
    case class Alternative(patterns: List[Pattern]) extends Pattern
  }
}

