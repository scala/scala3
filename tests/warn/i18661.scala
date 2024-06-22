class Jacket[T]:
  sealed trait BodyType:
    sealed trait OrganType:
      case class Heart() extends Body.Organ
      case class Brain() extends Body.Organ
    object Organ extends OrganType
    sealed trait Organ
  object Body extends BodyType
  sealed trait Body

type AnyJacket         = Jacket[?]
type AnyBodyOrgan      = AnyJacket#BodyType#Organ
type AnyBodyOrganHeart = AnyJacket#BodyType#OrganType#Heart
type AnyBodyOrganBrain = AnyJacket#BodyType#OrganType#Brain

def check( asr : AnyBodyOrgan ) : String =
  asr match
    case c : AnyBodyOrganHeart => "Heart"
    case s : AnyBodyOrganBrain => "Brain" // was: unreachable

val jacket = new Jacket[Unit]
val heart = new jacket.Body.Organ.Heart()
val brain = new jacket.Body.Organ.Brain()

@main
def go =
  println( check( heart ) )
  println( check( brain ) )
