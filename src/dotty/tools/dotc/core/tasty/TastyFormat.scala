package dotty.tools.dotc
package core
package tasty

/************************************************************
Notation:

We use BNF notation. Terminal symbols start with at least two
consecutive upper case letters. Each terminal is represented as a
single byte tag. Non-terminals are mixed case. Prefixes of the form
lower case letter*_ are for explanation of semantic content only, they
can be dropped without changing the grammar.

Micro-syntax:

  LongInt       = Digit* StopDigit        // big endian 2's complement, value fits in a Long w/o overflow
  Int           = LongInt                 // big endian 2's complement, fits in an Int w/o overflow
  Nat           = LongInt                 // non-negative value, fits in an Int without overflow
  Digit         = 0 | ... | 127
  StopDigit     = 128 | ... | 255         // value = digit - 128

Macro-format:

  File          = Header majorVersion_Nat minorVersion_Nat UUID
                  nameTable_Length Name* Section*
  Header        = 0x5CA1AB1F
  UUID          = Byte*16                // random UUID

  Section       = NameRef Length Bytes
  Length        = Nat                    // length of rest of entry in bytes

  Name          = UTF8           Length UTF8-CodePoint*
                  QUALIFIED      Length qualified_NameRef selector_NameRef
                  SIGNED         Length original_NameRef resultSig_NameRef paramSig_NameRef*
                  EXPANDED       Length original_NameRef
                  OBJECTCLASS    Length module_NameRef
                  SUPERACCESSOR  Length accessed_NameRef
                  DEFAULTGETTER  Length method_NameRef paramNumber_Nat
                  SHADOWED       Length original_NameRef
                  MANGLED        Length mangle_NameRef name_NameRef
                  ...

  NameRef       = Nat                    // ordinal number of name in name table, starting from 1.

Note: Unqualified names in the name table are strings. The context decides whether a name is
a type-name or a term-name. The same string can represent both.

Standard-Section: "ASTs" TopLevelStat*

  TopLevelStat  = PACKAGE        Length Path TopLevelStat*
                  Stat

  Stat          = Term
                  VALDEF         Length NameRef Type rhs_Term? Modifier*
                  DEFDEF         Length NameRef TypeParam* Params* return_Type rhs_Term?
                                        Modifier*
                  TYPEDEF        Length NameRef (Type | Template) Modifier*
                  IMPORT         Length qual_Term Selector*
  Selector      = IMPORTED              name_NameRef
                  RENAMED        Length from_NameRef to_NameRef
                                 // Imports are for scala.meta, they are not used in the backend

  TypeParam     = TYPEPARAM      Length NameRef Type Modifier*
  Params        = PARAMS         Length Param*
  Param         = PARAM          Length NameRef Type rhs_Term? Modifier*  // rhs_Term is present in the case of an aliased class parameter
  Template      = TEMPLATE       Length TypeParam* Param* Parent* Self? Stat* // Stat* always starts with the primary constructor.
  Parent        = Application
                  Type
  Self          = SELFDEF               selfName_NameRef selfType_Type

  Term          = Path
                  Application
                  IDENT                 NameRef Type     // used when ident’s type is not a TermRef
                  SELECT                possiblySigned_NameRef qual_Term
                  NEW                   cls_Type
                  SUPER          Length this_Term mixinTrait_Type?
                  PAIR           Length left_Term right_Term
                  TYPED          Length expr_Term ascription_Type
                  NAMEDARG       Length paramName_NameRef arg_Term
                  ASSIGN         Length lhs_Term rhs_Term
                  BLOCK          Length expr_Term Stat*
                  LAMBDA         Length meth_Term target_Type
                  IF             Length cond_Term then_Term else_Term
                  MATCH          Length sel_Term CaseDef*
                  TRY            Length expr_Term CaseDef* finalizer_Term?
                  RETURN         Length meth_ASTRef expr_Term?
                  REPEATED       Length elem_Type elem_Term*
                  BIND           Length boundName_NameRef patType_Type pat_Term
                  ALTERNATIVE    Length alt_Term*
                  UNAPPLY        Length fun_Term ImplicitArg* pat_Type pat_Term*
                  EMPTYTREE
                  SHARED                term_ASTRef
  Application   = APPLY          Length fn_Term arg_Term*

                  TYPEAPPLY      Length fn_Term arg_Type*
  CaseDef       = CASEDEF        Length pat_Term rhs_Tree guard_Tree?
  ImplicitArg   = IMPLICITARG           arg_Term
  ASTRef        = Nat                               // byte position in AST payload

  Path          = Constant
                  TERMREFdirect         sym_ASTRef
                  TERMREFsymbol         sym_ASTRef qual_Type
                  TERMREFpkg            fullyQualified_NameRef
                  TERMREF               possiblySigned_NameRef qual_Type
                  THIS                  clsRef_Type
                  RECthis               recType_ASTRef
                  SHARED                path_ASTRef

  Constant      = UNITconst
                  FALSEconst
                  TRUEconst
                  BYTEconst             Int
                  SHORTconst            Int
                  CHARconst             Nat
                  INTconst              Int
                  LONGconst             LongInt
                  FLOATconst            Int
                  DOUBLEconst           LongInt
                  STRINGconst           NameRef
                  NULLconst
                  CLASSconst            Type
                  ENUMconst             Path

  Type          = Path
                  TYPEREFdirect         sym_ASTRef
                  TYPEREFsymbol         sym_ASTRef qual_Type
                  TYPEREFpkg            fullyQualified_NameRef
                  TYPEREF               possiblySigned_NameRef qual_Type
                  RECtype               parent_Type
                  SUPERtype      Length this_Type underlying_Type
                  REFINEDtype    Length underlying_Type refinement_NameRef info_Type
                  APPLIEDtype    Length tycon_Type arg_Type*
                  TYPEBOUNDS     Length low_Type high_Type bindingKind_Nat?
                  TYPEALIAS      Length alias_Type (COVARIANT | CONTRAVARIANT)?
                  ANNOTATED      Length underlying_Type fullAnnotation_Term
                  ANDtype        Length left_Type right_Type
                  ORtype         Length left_Type right_Type
                  BIND           Length boundName_NameRef bounds_Type
                                        // for type-variables defined in a type pattern
                  BYNAMEtype            underlying_Type
                  POLYtype       Length result_Type NamesTypes      // needed for refinements
                  METHODtype     Length result_Type NamesTypes      // needed for refinements
                  PARAMtype      Length binder_ASTref paramNum_Nat  // needed for refinements
                  SHARED                type_ASTRef
  NamesTypes    = ParamType*
  NameType      = paramName_NameRef typeOrBounds_ASTRef

  Modifier      = PRIVATE
                  INTERNAL                        // package private
                  PROTECTED
                  PRIVATEqualified     qualifier_Type       // will be dropped
                  PROTECTEDqualified   qualifier_Type   // will be dropped
                  ABSTRACT
                  FINAL
                  SEALED
                  CASE
                  IMPLICIT
                  LAZY
                  OVERRIDE
                  INLINE                // macro
                  STATIC                            // mapped to static Java member
                  OBJECT                            // an object or its class
                  TRAIT                 // a trait
                  LOCAL                           // private[this] or protected[this]
                  SYNTHETIC                     // generated by Scala compiler
                  ARTIFACT                        // to be tagged Java Synthetic
                  MUTABLE                         // a var
                  LABEL                           // method generated as a label
                  FIELDaccessor               // getter or setter
                  CASEaccessor              // getter for case class param
                  COVARIANT                     // type param marked “+”
                  CONTRAVARIANT             // type param marked “-”
                  SCALA2X                           // Imported from Scala2.x
                  DEFAULTparameterized  // Method with default params
                  INSUPERCALL           // defined in the argument of a constructor supercall
                  STABLE                // Method that is assumed to be stable
                  Annotation
  Annotation    = ANNOTATION     Length tycon_Type fullAnnotation_Term

Note: Tree tags are grouped into 5 categories that determine what follows, and thus allow to compute the size of the tagged tree in a generic way.

  Category 1 (tags 0-63)   :  tag
  Category 2 (tags 64-95)  :  tag Nat
  Category 3 (tags 96-111) :  tag AST
  Category 4 (tags 112-127):  tag Nat AST
  Category 5 (tags 128-255):  tag Length <payload>

Standard Section: "Positions" sourceLength_Nat Assoc*

  Assoc         = addr_Delta offset_Delta offset_Delta?
                                            // addr_Delta      :
                                            //    Difference of address to last recorded node.
                                            //    All but the first addr_Deltas are > 0, the first is >= 0.
                                            // 2nd offset_Delta:
                                            //    Difference of end offset of addressed node vs parent node. Always <= 0
                                            // 1st offset Delta, if delta >= 0 or 2nd offset delta exists
                                            //    Difference of start offset of addressed node vs parent node.
                                            // 1st offset Delta, if delta < 0 and 2nd offset delta does not exist:
                                            //    Difference of end offset of addressed node vs parent node.
                                            // Offsets and addresses are difference encoded.
                                            // Nodes which have the same positions as their parents are omitted.
  Delta         = Int                       // Difference between consecutive offsets / tree addresses,

**************************************************************************************/

object TastyFormat {

  final val header = Array(0x5C, 0xA1, 0xAB, 0x1F)
  final val MajorVersion = 0
  final val MinorVersion = 5

  // Name tags

  final val UTF8 = 1
  final val QUALIFIED = 2
  final val SIGNED = 3
  final val EXPANDED = 4
  final val OBJECTCLASS = 5
  final val SUPERACCESSOR = 6
  final val DEFAULTGETTER = 7
  final val SHADOWED = 8

  // AST tags

  final val UNITconst = 2
  final val FALSEconst = 3
  final val TRUEconst = 4
  final val NULLconst = 5
  final val PRIVATE = 6
  final val INTERNAL = 7
  final val PROTECTED = 8
  final val ABSTRACT = 9
  final val FINAL = 10
  final val SEALED = 11
  final val CASE = 12
  final val IMPLICIT = 13
  final val LAZY = 14
  final val OVERRIDE = 15
  final val INLINE = 16
  final val STATIC = 17
  final val OBJECT = 18
  final val TRAIT = 19
  final val LOCAL = 20
  final val SYNTHETIC = 21
  final val ARTIFACT = 22
  final val MUTABLE = 23
  final val LABEL = 24
  final val FIELDaccessor = 25
  final val CASEaccessor = 26
  final val COVARIANT = 27
  final val CONTRAVARIANT = 28
  final val SCALA2X = 29
  final val DEFAULTparameterized = 30
  final val INSUPERCALL = 31
  final val STABLE = 32

  final val SHARED = 64
  final val TERMREFdirect = 65
  final val TYPEREFdirect = 66
  final val TERMREFpkg = 67
  final val TYPEREFpkg = 68
  final val REFINEDthis = 69
  final val RECthis = REFINEDthis // !!!
  final val BYTEconst = 70
  final val SHORTconst = 71
  final val CHARconst = 72
  final val INTconst = 73
  final val LONGconst = 74
  final val FLOATconst = 75
  final val DOUBLEconst = 76
  final val STRINGconst = 77
  final val IMPORTED = 78

  final val THIS = 96
  final val CLASSconst = 97
  final val ENUMconst = 98
  final val BYNAMEtype = 99
  final val NEW = 100
  final val IMPLICITarg = 101
  final val PRIVATEqualified = 102
  final val PROTECTEDqualified = 103
  final val RECtype = 104

  final val IDENT = 112
  final val SELECT = 113
  final val TERMREFsymbol = 114
  final val TERMREF = 115
  final val TYPEREFsymbol = 116
  final val TYPEREF = 117
  final val SELFDEF = 118

  final val PACKAGE = 128
  final val VALDEF = 129
  final val DEFDEF = 130
  final val TYPEDEF = 131
  final val IMPORT = 132
  final val TYPEPARAM = 133
  final val PARAMS = 134
  final val PARAM = 136
  final val RENAMED = 138
  final val APPLY = 139
  final val TYPEAPPLY = 140
  final val PAIR = 142
  final val TYPED = 143
  final val NAMEDARG = 144
  final val ASSIGN = 145
  final val BLOCK = 146
  final val IF = 147
  final val LAMBDA = 148
  final val MATCH = 149
  final val RETURN = 150
  final val TRY = 151
  final val REPEATED = 153
  final val BIND = 154
  final val ALTERNATIVE = 155
  final val UNAPPLY = 156
  final val ANNOTATED = 157
  final val CASEDEF = 158
  final val TEMPLATE = 160
  final val SUPER = 163
  final val SUPERtype = 166
  final val REFINEDtype = 167
  final val APPLIEDtype = 168
  final val TYPEBOUNDS = 169
  final val TYPEALIAS = 170
  final val ANDtype = 171
  final val ORtype = 172
  final val METHODtype = 174
  final val POLYtype = 175
  final val PARAMtype = 176
  final val ANNOTATION = 178

  final val firstSimpleTreeTag = UNITconst
  final val firstNatTreeTag = SHARED
  final val firstASTTreeTag = THIS
  final val firstNatASTTreeTag = IDENT
  final val firstLengthTreeTag = PACKAGE

  def isParamTag(tag: Int) = tag == PARAM || tag == TYPEPARAM

  def isModifierTag(tag: Int) = tag match {
    case PRIVATE
       | INTERNAL
       | PROTECTED
       | ABSTRACT
       | FINAL
       | SEALED
       | CASE
       | IMPLICIT
       | LAZY
       | OVERRIDE
       | INLINE
       | STATIC
       | OBJECT
       | TRAIT
       | LOCAL
       | SYNTHETIC
       | ARTIFACT
       | MUTABLE
       | LABEL
       | FIELDaccessor
       | CASEaccessor
       | COVARIANT
       | CONTRAVARIANT
       | SCALA2X
       | DEFAULTparameterized
       | INSUPERCALL
       | STABLE
       | ANNOTATION
       | PRIVATEqualified
       | PROTECTEDqualified => true
    case _ => false
   }

  def nameTagToString(tag: Int): String = tag match {
    case UTF8 => "UTF8"
    case QUALIFIED => "QUALIFIED"
    case SIGNED => "SIGNED"
    case EXPANDED => "EXPANDED"
    case OBJECTCLASS => "OBJECTCLASS"
    case SUPERACCESSOR => "SUPERACCESSOR"
    case DEFAULTGETTER => "DEFAULTGETTER"
  }

  def astTagToString(tag: Int): String = tag match {
    case UNITconst => "UNITconst"
    case FALSEconst => "FALSEconst"
    case TRUEconst => "TRUEconst"
    case NULLconst => "NULLconst"
    case PRIVATE => "PRIVATE"
    case INTERNAL => "INTERNAL"
    case PROTECTED => "PROTECTED"
    case ABSTRACT => "ABSTRACT"
    case FINAL => "FINAL"
    case SEALED => "SEALED"
    case CASE => "CASE"
    case IMPLICIT => "IMPLICIT"
    case LAZY => "LAZY"
    case OVERRIDE => "OVERRIDE"
    case INLINE => "INLINE"
    case STATIC => "STATIC"
    case OBJECT => "OBJECT"
    case TRAIT => "TRAIT"
    case LOCAL => "LOCAL"
    case SYNTHETIC => "SYNTHETIC"
    case ARTIFACT => "ARTIFACT"
    case MUTABLE => "MUTABLE"
    case LABEL => "LABEL"
    case FIELDaccessor => "FIELDaccessor"
    case CASEaccessor => "CASEaccessor"
    case COVARIANT => "COVARIANT"
    case CONTRAVARIANT => "CONTRAVARIANT"
    case SCALA2X => "SCALA2X"
    case DEFAULTparameterized => "DEFAULTparameterized"
    case INSUPERCALL => "INSUPERCALL"
    case STABLE => "STABLE"

    case SHARED => "SHARED"
    case TERMREFdirect => "TERMREFdirect"
    case TYPEREFdirect => "TYPEREFdirect"
    case TERMREFpkg => "TERMREFpkg"
    case TYPEREFpkg => "TYPEREFpkg"
    case RECthis => "RECthis"
    case BYTEconst => "BYTEconst"
    case SHORTconst => "SHORTconst"
    case CHARconst => "CHARconst"
    case INTconst => "INTconst"
    case LONGconst => "LONGconst"
    case FLOATconst => "FLOATconst"
    case DOUBLEconst => "DOUBLEconst"
    case STRINGconst => "STRINGconst"
    case RECtype => "RECtype"

    case IDENT => "IDENT"
    case SELECT => "SELECT"
    case TERMREFsymbol => "TERMREFsymbol"
    case TERMREF => "TERMREF"
    case TYPEREFsymbol => "TYPEREFsymbol"
    case TYPEREF => "TYPEREF"

    case PACKAGE => "PACKAGE"
    case VALDEF => "VALDEF"
    case DEFDEF => "DEFDEF"
    case TYPEDEF => "TYPEDEF"
    case IMPORT => "IMPORT"
    case TYPEPARAM => "TYPEPARAM"
    case PARAMS => "PARAMS"
    case PARAM => "PARAM"
    case IMPORTED => "IMPORTED"
    case RENAMED => "RENAMED"
    case APPLY => "APPLY"
    case TYPEAPPLY => "TYPEAPPLY"
    case NEW => "NEW"
    case PAIR => "PAIR"
    case TYPED => "TYPED"
    case NAMEDARG => "NAMEDARG"
    case ASSIGN => "ASSIGN"
    case BLOCK => "BLOCK"
    case IF => "IF"
    case LAMBDA => "LAMBDA"
    case MATCH => "MATCH"
    case RETURN => "RETURN"
    case TRY => "TRY"
    case REPEATED => "REPEATED"
    case BIND => "BIND"
    case ALTERNATIVE => "ALTERNATIVE"
    case UNAPPLY => "UNAPPLY"
    case ANNOTATED => "ANNOTATED"
    case CASEDEF => "CASEDEF"
    case IMPLICITarg => "IMPLICITarg"
    case TEMPLATE => "TEMPLATE"
    case SELFDEF => "SELFDEF"
    case THIS => "THIS"
    case SUPER => "SUPER"
    case CLASSconst => "CLASSconst"
    case ENUMconst => "ENUMconst"
    case SUPERtype => "SUPERtype"
    case REFINEDtype => "REFINEDtype"
    case APPLIEDtype => "APPLIEDtype"
    case TYPEBOUNDS => "TYPEBOUNDS"
    case TYPEALIAS => "TYPEALIAS"
    case ANDtype => "ANDtype"
    case ORtype => "ORtype"
    case BYNAMEtype => "BYNAMEtype"
    case POLYtype => "POLYtype"
    case METHODtype => "METHODtype"
    case PARAMtype => "PARAMtype"
    case ANNOTATION => "ANNOTATION"
    case PRIVATEqualified => "PRIVATEqualified"
    case PROTECTEDqualified => "PROTECTEDqualified"
  }

  /** @return If non-negative, the number of leading references of a length/trees entry.
   *          If negative, minus the number of leading non-reference trees.
   */
  def numRefs(tag: Int) = tag match {
    case VALDEF | DEFDEF | TYPEDEF | TYPEPARAM | PARAM | NAMEDARG | RETURN | BIND |
         SELFDEF | REFINEDtype => 1
    case RENAMED | PARAMtype => 2
    case POLYtype | METHODtype => -1
    case TYPEBOUNDS => -2
    case _ => 0
  }
}
