package scala.quoted

/** Current QuoteContext.reflect in scope
 *
 *  Usage:
 *  To use reflection inside a method
 *  ```
 *  def f(using QuoteContext) = { // or (using qctx: QuoteContext)
 *    import reflect._ // equivalent to import reflect._
 *    ...
 *  }
 *  ```
 *  or to use reflection in the signature of a method
 *  ```
 *  def f(using QuoteContext)(tree: reflect.Tree): reflect.Position = ...
 *  ```
 */
def reflect(using qctx: QuoteContext): qctx.reflect.type = qctx.reflect
