class Inv[T]
class Cov[+T]
class Contra[-T]

// Nesting captures in non-covariant position

type InvNesting[X] = X match // error
  case Inv[Cov[t]] => t

type ContraNesting[X] = X match // error
  case Contra[Cov[t]] => t

// Intersection type to type-test and capture at the same time

type AndTypeMT[X] = X match // error
  case t & Seq[Any] => t

// Poly type alias with a bound to type-test and capture at the same time

type IsSeq[X <: Seq[Any]] = X

type TypeAliasWithBoundMT[X] = X match // error
  case IsSeq[t] => t

// Poly type alias with an unknown type member refinement

type TypeMemberAux[X] = { type TypeMember = X }

type TypeMemberExtractorMT[X] = X match // error
  case TypeMemberAux[t] => t

// Poly type alias with a refined member of stronger bounds than in the parent

class Base {
  type TypeMember
}

type TypeMemberAux2[X <: Seq[Any]] = Base { type TypeMember = X }

type TypeMemberExtractorMT2[X] = X match // error
  case TypeMemberAux2[t] => t
