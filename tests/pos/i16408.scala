//> using options -source:3.3

import scala.util.Try

trait RDF:
  rdf =>

  type R = rdf.type
  type Node <: Matchable
  type URI <: Node

  given rops: ROps[R]
end RDF

object RDF:
  type Node[R <: RDF] = R match
    case GetNode[n] => Matchable //n & rNode[R]

  type URI[R <: RDF] <: Node[R] = R match
    case GetURI[u] => u & Node[R]

  private type GetNode[N] = RDF { type Node = N }
  private type GetURI[U] = RDF { type URI = U }
end RDF

trait ROps[R <: RDF]:
  def mkUri(str: String): Try[RDF.URI[R]]
  def auth(uri: RDF.URI[R]): Try[String]

object TraitTypes:
  trait Node:
    def value: String

  trait Uri extends Node

  def mkUri(u: String): Uri =
    new Uri { def value = u }

object TraitRDF extends RDF:
  import TraitTypes as tz

  override opaque type Node <: Matchable = tz.Node
  override opaque type URI <: Node = tz.Uri

  given rops: ROps[R] with
    override def mkUri(str: String): Try[RDF.URI[R]] = Try(tz.mkUri(str))
    override def auth(uri: RDF.URI[R]): Try[String] =
      Try(java.net.URI.create(uri.value).getAuthority())

end TraitRDF

class Test[R <: RDF](using rops: ROps[R]):
  import rops.given
  lazy val uriT: Try[RDF.URI[R]] = rops.mkUri("https://bblfish.net/#i")
  lazy val x: String = "uri authority=" + uriT.map(u => rops.auth(u))

@main def run =
  val test = Test[TraitRDF.type]
  println(test.x)
