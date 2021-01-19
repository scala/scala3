class Animal
class Dog extends Animal

enum Opt[+T] with
  case Sm(t: T)
  case None

val smDog: Opt.Sm[Dog] = new Opt.Sm(Dog())
val smAnimal: Opt.Sm[Animal] = smDog

enum Show[-T] with
  case Refl(op: T => String)

  def show(t: T): String = this match
    case Refl(op) => op(t)

val reflAnimal: Show.Refl[Animal] = new Show.Refl(_.toString)
val reflDog: Show.Refl[Dog] = reflAnimal
