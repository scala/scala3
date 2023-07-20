object Helpers:
  type NodeFun[R] = Matchable // compiles without [R] parameter

  type URIFun[R] = R match
    case RDF[u] => u & NodeFun[R]
end Helpers

trait RDF[URIParam]

trait ROps[R <: RDF[?]]:
  def auth(uri: Helpers.URIFun[R]): String

object TraitRDF extends RDF[TraitTypes.UriImpl]:

  val rops = new ROps[TraitRDF.type] {
    override def auth(uri: Helpers.URIFun[TraitRDF.type]): String = ???
  }
end TraitRDF

object TraitTypes:
  trait UriImpl // doesn't compile
  // class UriImpl // compiles
