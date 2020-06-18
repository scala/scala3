trait Resolver:
  def resolve(label: String): Any

class ResolvingSelectable extends Selectable:
  def selectDynamic(label: String)(using r: Resolver): Any =
    r.resolve(label)
  def applyDynamic(label: String)(args: Any*)(using r: Resolver): Any =
    r.resolve(label).asInstanceOf[Any => Any].apply(args.head)

type Person = ResolvingSelectable {
  val name: String
  val age: Int
  def likes(other: String): Boolean
}


@main def Test =
  given Resolver:
    def resolve(label: String) = label match
      case "name" => "Emma"
      case "age" => 8
      case "likes" => (food: String) => food == "Cake"

  val emma = ResolvingSelectable().asInstanceOf[Person]

  assert(emma.name == "Emma")
  assert(emma.age == 8)
  assert(emma.likes("Cake"))
