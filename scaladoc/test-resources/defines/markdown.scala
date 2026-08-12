package pkg
// we don't want the asterisks from Markdown bolding to be confused with Scaladoc comment prefixes
/**
 * @define x **middle**
 */
class C {
  /** before $x after */
  def foo(): Int = 0
}