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

// Poly type alias with a type member refinement to extract the type member

class Base {
  type TypeMember
}

type TypeMemberAux[X] = Base { type TypeMember = X }

type TypeMemberExtractorMT[X] = X match // error
  case TypeMemberAux[t] => t
