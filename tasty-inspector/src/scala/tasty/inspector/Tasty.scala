package scala.tasty.inspector

import scala.quoted._

/** `.tasty` file representation containing file path and the AST */
trait Tasty[Q <: Quotes & Singleton]:

  /** Instance of `Quotes` used to load the AST */
  val quotes: Q

  /** Path to the `.tasty` file */
  def path: String

  /** Abstract Syntax Tree contained in the `.tasty` file */
  def ast: quotes.reflect.Tree

end Tasty
