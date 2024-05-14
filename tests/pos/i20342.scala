class Repo[EC, E](using defaults: RepoDefaults[EC, E])
trait RepoDefaults[EC, E]
object RepoDefaults:
  inline given genImmutableRepo[E: DbCodec]: RepoDefaults[E, E] = ???
  inline given genRepo[EC: DbCodec, E: DbCodec]: RepoDefaults[EC, E] = ???

trait DbCodec[E]

case class PersonCreator(name: String)
case class Person(id: Long)
given DbCodec[Person] = ???
given DbCodec[PersonCreator] = ???

@main def Test =
  val personRepo = Repo[PersonCreator, Person]
