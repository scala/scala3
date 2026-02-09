class Generator:
  private def generateTable(table: Table) =
    val (ownRelations, _) = calculateOwnRelations(table)
    ownRelations

  private def calculateOwnRelations(table: Table) =
    val ownRelations = table.relations.filter(_.association.isDefined)
    (ownRelations, Nil)

case class Table(relations: Seq[TableRelation])
case class TableRelation(association: Option[Association])
trait Association
