import scala.quoted.*

def qwe(using Quotes) = {
  import quotes.reflect.*

  def ko_1(param: ValDef | TypeDef) =
    param match {
      case _: ValDef =>
      case _: TypeDef =>
    }

  def ko_2(params: List[ValDef] | List[TypeDef]) =
    params.map {
      case x: ValDef =>
      case y: TypeDef =>
    }

  def ko_3(param: ValDef | TypeDef) =
    param match {
      case _: ValDef =>
    }
}
