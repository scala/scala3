// Similar to i24207

class Generator:
  private def generateTable(table: Table) =
    val (ownRelations, unusedTerm) = calculateOwnRelations(table)
    None

  private def calculateOwnRelations(table: Table) =
    val ownRelations = table.relations.filter(_.association.isDefined)
    (ownRelations, Nil)

case class Table(relations: Seq[TableRelation])
case class TableRelation(association: Option[Association])
trait Association