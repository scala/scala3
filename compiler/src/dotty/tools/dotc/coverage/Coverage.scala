package dotty.tools.dotc
package coverage

import scala.collection.mutable

class Coverage {
  private val statementsById = mutable.Map[Int, Statement]()

  def statements = statementsById.values

  def addStatement(stmt: Statement): Unit = statementsById.put(stmt.id, stmt)
}

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
) {
  def invoked(): Unit = count = count + 1
  def isInvoked = count > 0
}
