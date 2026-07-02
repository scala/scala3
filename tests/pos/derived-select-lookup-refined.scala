trait DerivedSelectBase:
  type A
  type B
  type C
  val tag: String

type DirectRefinement =
  DerivedSelectBase { type A = String; type B = List[A]; val tag: "direct" }

type AliasRefinement = DirectRefinement

type RecursiveRefinement =
  DerivedSelectBase { type A = String; type B = A; type C = B }

def directRefined(x: DirectRefinement): x.A =
  val value: String = ""
  value

def aliasRefined(x: AliasRefinement): x.B =
  List("")

def recursiveRefined(x: RecursiveRefinement): x.C =
  val value: String = ""
  value

trait StableOuter:
  type Elem
  val member: DerivedSelectBase { type A = Elem; type B = List[A]; val tag: "stable" }

def singletonPrefix(o: StableOuter { type Elem = Int }): o.member.B =
  List(1)

trait UnstableOuter:
  type Elem
  def member: DerivedSelectBase { type A = Elem; type B = List[A]; val tag: "unstable" }

def useSkolemPrefix(x: DerivedSelectBase { type A = Boolean }): x.A =
  true

def skolemPrefix(o: UnstableOuter { type Elem = Boolean }): Boolean =
  useSkolemPrefix(o.member)

def inferredTypeParam[T <: DerivedSelectBase { type A = Char; type B = List[A] }](x: T): x.B =
  val value: x.A = 'a'
  List(value)
