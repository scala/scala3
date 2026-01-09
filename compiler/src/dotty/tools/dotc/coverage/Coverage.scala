package dotty.tools.dotc
package coverage

import scala.collection.mutable
import java.nio.file.Path

/** Holds a list of statements to include in the coverage reports. */
class Coverage:
  private val statementsById = new mutable.LongMap[Statement](256)

  private var _nextStatementId: Int = 0

  def nextStatementId(): Int = _nextStatementId
  def setNextStatementId(id: Int): Unit = _nextStatementId = id

  def statements: Iterable[Statement] = statementsById.values

  def addStatement(stmt: Statement): Unit =
    if stmt.id >= _nextStatementId then _nextStatementId = stmt.id + 1
    statementsById(stmt.id) = stmt

/**
  * A statement that can be invoked, and thus counted as "covered" by code coverage tools.
  *
  * @param line 1-indexed line number
  */
case class Statement(
    location: Location,
    id: Int,
    start: Int,
    end: Int,
    line: Int,
    desc: String,
    symbolName: String,
    treeName: String,
    branch: Boolean,
    ignored: Boolean = false
)