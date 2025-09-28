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