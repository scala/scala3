//> using options -source:3.3

object Helpers:
  type NodeFun[R] = Matchable // compiles without [R] parameter

  type URIFun[R] = R match
    case GetURI[u] => u & NodeFun[R]

  private type GetURI[U] = RDF { type URI = U }
end Helpers

trait RDF:
  type URI

trait ROps[R <: RDF]:
  def auth(uri: Helpers.URIFun[R]): String

object TraitRDF extends RDF:
  override type URI = TraitTypes.UriImpl

  val rops = new ROps[TraitRDF.type] {
    override def auth(uri: Helpers.URIFun[TraitRDF.type]): String = ???
  }
end TraitRDF

object TraitTypes:
  trait UriImpl // doesn't compile
  // class UriImpl // compiles
