package dotty.tools.dotc
package coverage

import scala.collection.mutable

/** Holds a list of statements to include in the coverage reports. */
class Coverage:
  private val statementsById = new mutable.LongMap[Statement](256)

  def statements: Iterable[Statement] = statementsById.values

  def addStatement(stmt: Statement): Unit = statementsById(stmt.id) = stmt

/** A statement that can be invoked, and thus counted as "covered" by code coverage tools. */
case class Statement(
    source: String,
    location: Location,
    id: Int,
    start: Int,
    end: Int,
    line: Int,
    desc: String,
    symbolName: String,
    treeName: String,
    branch: Boolean,
    var count: Int = 0,
    ignored: Boolean = false
):
  /** Records that this statement has been invoked one more time. */
  def invoked(): Unit =
    count += 1

  def isInvoked: Boolean =
    count > 0
