class Animal
class Dog extends Animal

enum Opt[+T]:
  case Sm(t: T)
  case None

val smDog: Opt.Sm[Dog] = new Opt.Sm(Dog())
val smAnimal: Opt.Sm[Animal] = smDog

enum Show[-T]:
  case Refl(op: T => String)

  def show(t: T): String = this match
    case Refl(op) => op(t)

val reflAnimal: Show.Refl[Animal] = new Show.Refl(_.toString)
val reflDog: Show.Refl[Dog] = reflAnimal
