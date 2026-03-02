// i24101

trait AttributeValue

case class CompletedSpan(
  name: String, // remove to make it compile
  attributes: MyMap[String, AttributeValue],
){
  lazy val allAttributes: MyMap[String, AttributeValue] = attributes
  var allAttributes2: MyMap[String, AttributeValue] = attributes
}

def Test =
  val span: CompletedSpan = ???
  span.copy(attributes = span.allAttributes.filterNot { _ => false })
  span.copy(attributes = span.allAttributes2.filterNot { _ => false })

// i24100

trait Type

type ColumnDef = ColumnDef_[Type]
case class ColumnDef_[+T](comments: String)

type TableDef = TableDef_[ColumnDef]
case class TableDef_[+C <: ColumnDef_[?]](cols: MySeq[C])

abstract class DdlGenerator:
  // The result type is inferred here
  def columnComments(t: TableDef) = t.cols.map(_ => "")

class CassandraDdlGenerator() extends DdlGenerator:
  // The result type should be inferred here as well
  override def columnComments(t: TableDef) = ???