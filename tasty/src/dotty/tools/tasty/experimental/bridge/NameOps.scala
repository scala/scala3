package dotty.tools.tasty.experimental.bridge

import reflect.ClassTag

trait NameOps extends Core with

  given ClassTag[Name] = internal.Name_CT
  given ClassTag[SimpleName] = internal.SimpleName_CT
  given ClassTag[DerivedName] = internal.DerivedName_CT
  given ClassTag[TypeName] = internal.TypeName_CT
  given ClassTag[TermName] = internal.TermName_CT

  object nme {
    val WILDCARD: TermName = internal.nme_WILDCARD
  }

  object SignedName with
    def unapply(name: DerivedName): Option[(TermName, Signature)] = internal.SignedName_unapply(name)
    def apply(name: TermName, sig: Signature): TermName = internal.SignedName_apply(name, sig)

  object AnyQualifiedName with
    def unapply(name: DerivedName): Option[(TermName, SimpleName)] = internal.AnyQualifiedName_unapply(name)

  /** An extractor for unique names of arbitrary kind */
  object AnyUniqueName with
    def unapply(name: DerivedName): Option[(TermName, String, Int)] = internal.AnyUniqueName_unapply(name)

  object AnyNumberedName with
    def unapply(name: DerivedName): Option[(TermName, Int)] = internal.AnyNumberedName_unapply(name)

  object DerivedName with
    def unapply(name: DerivedName): Some[TermName] = internal.DerivedName_unapply(name)

  object OuterSelectName with
    def unapply(name: DerivedName): Option[(TermName, Int)] = internal.OuterSelectName_unapply(name)


  given NameOps: (name: Name) with
    def toTermName: TermName = internal.Name_toTermName(name)
    def isEmpty: Boolean = internal.Name_isEmpty(name)
    def isTypeName: Boolean = internal.Name_isTypeName(name)
    def isTermName: Boolean = !name.isTypeName

  given TermNameOps: (name: TermName) with
    def tag: Int = internal.TermName_tag(name)

  given SimpleNameOps: (name: SimpleName) with
    def toUTF8: Array[Byte] = internal.SimpleName_toUTF8(name)
