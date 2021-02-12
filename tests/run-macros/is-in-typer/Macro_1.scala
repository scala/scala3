import scala.quoted._

inline def isWhileTyping: Boolean = ${ whileTypeing }

transparent inline def isWhileTypingTransparent: Boolean = ${ whileTypeing }

private def whileTypeing(using Quotes): Expr[Boolean] =
  import quotes.reflect._
  Expr(CompilationInfo.isWhileTyping)
