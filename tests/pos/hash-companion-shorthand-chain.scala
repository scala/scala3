import scala.language.experimental.hashCompanionShorthand

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

  // Hierarchical chain: `#Mammal` is the shorthand, `.Dog` is plain Select.
  val a1: String = describe(#Mammal.Dog)
  val a2: Animal = #Bird.Robin

  // Application form: `#method(args)`.
  trait Frag
  object Frag:
    def color(name: String): Frag = new Frag {}
    def text(s: String): Frag    = new Frag {}

  def render(frag: Frag): String = frag.toString

  val r1: String = render(#color("red"))
  val r2: Frag   = #text("hi")

  // Mixed: shorthand + chain + apply.
  trait Builder
  object Builder:
    def of(name: String): Inner = new Inner
    class Inner:
      def withCount(n: Int): Builder = new Builder {}

  def build(b: Builder): String = b.toString
  val b1: String = build(#of("widget").withCount(3))

  // Method-chain continuation is unaffected by `#` (unlike the `.X` proposal).
  // `expr\n  .toUpperCase` still chains — the `#` sigil leaves the existing
  // chain-continuation rule untouched.
  val expr: String = "abc"
    .toUpperCase
    .reverse
