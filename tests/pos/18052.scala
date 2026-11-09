import scala.util.Try
import scala.reflect.TypeTest

trait RDF:
  rdf =>

  type R = rdf.type
  type Node <: Matchable
  type URI <: Node 
  type BNode <: Node 

  given rops: ROps[R]
end RDF

object RDF:
  type Node[R <: RDF] = R match
    case GetNode[n] => Matchable //n & rNode[R]

  type URI[R <: RDF] <: Node[R] = R match
    case GetURI[u] => u & Node[R] 

  type BNode[R <: RDF] <: Node[R] = R match
    case GetBNode[b] => b & Node[R] 

  type Subject[R <: RDF] = URI[R] | BNode[R]  

  private type GetNode[N] = RDF { type Node = N }
  private type GetURI[U] = RDF { type URI = U }
  private type GetBNode[B] = RDF { type BNode = B }
end RDF

trait ROps[R <: RDF]:
  def mkUri(str: String): Try[RDF.URI[R]]
  def auth(uri: RDF.URI[R]): Try[String]
  given subjToURITT: TypeTest[RDF.Subject[R], RDF.URI[R]]

object TraitTypes:
  trait Node:
    def value: String

  trait Uri extends Node
  trait BNode extends Node
  
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

    given subjToURITT: TypeTest[RDF.Subject[R], RDF.URI[R]] with
       override def unapply(s: RDF.Subject[R]): Option[s.type & URI] =
         s match
           case x: (s.type & RDF.URI[R]) => Some(x)
           case _                        => None
  
end TraitRDF

class Test[R <: RDF](using rops: ROps[R]):
  import rops.given
  lazy val uriT: Try[RDF.URI[R]] = rops.mkUri("https://bblfish.net/#i")
  lazy val x: String = "uri authority=" + uriT.map(u => rops.auth(u))

@main def run =
  val test = Test[TraitRDF.type]
  println(test.x)
