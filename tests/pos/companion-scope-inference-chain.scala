import scala.language.experimental.companionScopeInference

object Chain:

  sealed trait Animal
  object Animal:
    sealed trait Mammal extends Animal
    object Mammal:
      case object Dog extends Mammal
      case object Cat extends Mammal
    sealed trait Bird extends Animal
    object Bird:
      case object Robin extends Bird

  def describe(a: Animal): String = a.toString

  // Hierarchical chain: `Mammal` is not in scope, but Animal's companion has
  // it; companion scope inference resolves Mammal → Animal.Mammal. `.Dog` is then
  // plain Select on Animal.Mammal.
  val a1: String = describe(Mammal.Dog)
  val a2: Animal = Bird.Robin

  // Application form: `method(args)` where method is in the companion.
  trait Frag
  object Frag:
    def color(name: String): Frag = new Frag {}
    def text(s: String): Frag    = new Frag {}

  def render(frag: Frag): String = frag.toString

  val r1: String = render(color("red"))
  val r2: Frag   = text("hi")

  // Mixed: inferred + chain + apply.
  trait Builder
  object Builder:
    def of(name: String): Inner = new Inner
    class Inner:
      def withCount(n: Int): Builder = new Builder {}

  def build(b: Builder): String = b.toString
  val b1: String = build(of("widget").withCount(3))
