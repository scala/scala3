case class Table(owner: Owner.Id)

case class Owner(owner: Owner.Id) // type Id is not a member of object Playground.Owner

trait Typed[Tag] {
  type Id = String
}

object Owner extends Typed[Owner] {
  //type Id = String
}