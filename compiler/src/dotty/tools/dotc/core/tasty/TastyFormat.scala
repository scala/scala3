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

  Name          = UTF8              Length UTF8-CodePoint*
                  QUALIFIED         Length qualified_NameRef selector_NameRef
                  FLATTENED         Length qualified_NameRef selector_NameRef
                  EXPANDED          Length qualified_NameRef selector_NameRef
                  EXPANDEDPREFIX    Length qualified_NameRef selector_NameRef
                  TRAITSETTER       Length qualified_NameRef selector_NameRef
                  UNIQUE            Length separator_NameRef uniqid_Nat underlying_NameRef?
                  DEFAULTGETTER     Length underlying_NameRef index_Nat
                  VARIANT           Length underlying_NameRef variance_Nat      // 0: Contravariant, 1: Covariant
                  OUTERSELECT       Length underlying_NameRef nhops_Nat         // a reference to `nhops` <outer> selections, followed by `underlying`

                  SUPERACCESSOR     Length underlying_NameRef
                  PROTECTEDACCESSOR Length underlying_NameRef
                  PROTECTEDSETTER   Length underlying_NameRef
                  INITIALIZER       Length underlying_NameRef
                  SHADOWED          Length underlying_NameRef
                  AVOIDCLASH        Length underlying_NameRef
                  DIRECT            Length underlying_NameRef
                  FIELD             Length underlying_NameRef
                  EXTMETH           Length underlying_NameRef
                  OBJECTVAR         Length underlying_NameRef
                  OBJECTCLASS       Length underlying_NameRef
                  SIGNED            Length original_NameRef resultSig_NameRef paramSig_NameRef*

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
                  RENAMED               to_NameRef

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
                  IDENT                 NameRef Type     // used when term ident’s type is not a TermRef
                  SELECT                possiblySigned_NameRef qual_Term
                  QUALTHIS              typeIdent_Tree
                  NEW                   cls_Type
                  SUPER          Length this_Term mixinTypeIdent_Tree?
                  TYPED          Length expr_Term ascription_Type
                  NAMEDARG       Length paramName_NameRef arg_Term
                  ASSIGN         Length lhs_Term rhs_Term
                  BLOCK          Length expr_Term Stat*
                  INLINED        Length call_Term expr_Term Stat*
                  LAMBDA         Length meth_Term target_Type
                  IF             Length cond_Term then_Term else_Term
                  MATCH          Length sel_Term CaseDef*
                  TRY            Length expr_Term CaseDef* finalizer_Term?
                  RETURN         Length meth_ASTRef expr_Term?
                  REPEATED       Length elem_Type elem_Term*
                  BIND           Length boundName_NameRef patType_Type pat_Term
                  ALTERNATIVE    Length alt_Term*
                  UNAPPLY        Length fun_Term ImplicitArg* pat_Type pat_Term*
                  IDENTtpt              NameRef Type      // used for all type idents
                  SELECTtpt             NameRef qual_Term
                  SINGLETONtpt          Path
                  REFINEDtpt     Length underlying_Term refinement_Stat*
                  APPLIEDtpt     Length tycon_Term arg_Term*
                  POLYtpt        Length TypeParam* body_Term
                  TYPEBOUNDStpt  Length low_Term high_Term
                  ANNOTATEDtpt   Length underlying_Term fullAnnotation_Term
                  ANDtpt         Length left_Term right_Term
                  ORtpt          Length left_Term right_Term
                  BYNAMEtpt             underlying_Term
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
                  TYPEBOUNDS     Length low_Type high_Type
                  TYPEALIAS      Length alias_Type (COVARIANT | CONTRAVARIANT)?
                  ANNOTATEDtype  Length underlying_Type fullAnnotation_Term
                  ANDtype        Length left_Type right_Type
                  ORtype         Length left_Type right_Type
                  BIND           Length boundName_NameRef bounds_Type
                                        // for type-variables defined in a type pattern
                  BYNAMEtype            underlying_Type
                  POLYtype       Length result_Type NamesTypes
                  METHODtype     Length result_Type NamesTypes      // needed for refinements
                  TYPELAMBDAtype Length result_Type NamesTypes      // variance encoded in front of name: +/-/(nothing)
                  PARAMtype      Length binder_ASTref paramNum_Nat  // needed for refinements
                  SHARED                type_ASTRef
  NamesTypes    = NameType*
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
                  STABLE                // Method that is assumed to be stable
                  Annotation
  Annotation    = ANNOTATION     Length tycon_Type fullAnnotation_Term

Note: Tree tags are grouped into 5 categories that determine what follows, and thus allow to compute the size of the tagged tree in a generic way.

  Category 1 (tags 0-63)   :  tag
  Category 2 (tags 64-95)  :  tag Nat
  Category 3 (tags 96-111) :  tag AST
  Category 4 (tags 112-127):  tag Nat AST
  Category 5 (tags 128-255):  tag Length <payload>

Standard Section: "Positions" Assoc*

  Assoc         = Header offset_Delta? offset_Delta?
  Header        = addr_Delta +              // in one Nat: difference of address to last recorded node << 2 +
                  hasStartDiff +            // one bit indicating whether there follows a start address delta << 1
                  hasEndDiff                // one bit indicating whether there follows an end address delta
                                            // Nodes which have the same positions as their parents are omitted.
                                            // offset_Deltas give difference of start/end offset wrt to the
                                            // same offset in the previously recorded node (or 0 for the first recorded node)
  Delta         = Int                       // Difference between consecutive offsets,

**************************************************************************************/

object TastyFormat {

  final val header = Array(0x5C, 0xA1, 0xAB, 0x1F)
  final val MajorVersion = 0
  final val MinorVersion = 5

  // Name tags

  final val UTF8 = 1
  final val QUALIFIED = 2
  final val FLATTENED = 3
  final val EXPANDED = 4
  final val EXPANDPREFIX = 5
  final val TRAITSETTER = 6
  final val UNIQUE = 10
  final val DEFAULTGETTER = 11
  final val VARIANT = 12
  final val OUTERSELECT = 13

  final val SUPERACCESSOR = 20
  final val PROTECTEDACCESSOR = 21
  final val PROTECTEDSETTER = 22
  final val INITIALIZER = 23
  final val SHADOWED = 24
  final val AVOIDCLASH = 30
  final val DIRECT = 31
  final val FIELD = 32
  final val EXTMETH = 33
  final val OBJECTVAR = 39
  final val OBJECTCLASS = 40

  final val SIGNED = 63
  
  final val firstInternalTag = 64
  final val IMPLMETH = 64

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
  final val STABLE = 31

  final val SHARED = 64
  final val TERMREFdirect = 65
  final val TYPEREFdirect = 66
  final val TERMREFpkg = 67
  final val TYPEREFpkg = 68
  final val RECthis = 69
  final val BYTEconst = 70
  final val SHORTconst = 71
  final val CHARconst = 72
  final val INTconst = 73
  final val LONGconst = 74
  final val FLOATconst = 75
  final val DOUBLEconst = 76
  final val STRINGconst = 77
  final val IMPORTED = 78
  final val RENAMED = 79

  final val THIS = 96
  final val QUALTHIS = 97
  final val CLASSconst = 98
  final val ENUMconst = 99
  final val BYNAMEtype = 100
  final val BYNAMEtpt = 101
  final val NEW = 102
  final val IMPLICITarg = 103
  final val PRIVATEqualified = 104
  final val PROTECTEDqualified = 105
  final val RECtype = 106
  final val SINGLETONtpt = 107

  final val IDENT = 112
  final val IDENTtpt = 113
  final val SELECT = 114
  final val SELECTtpt = 115
  final val TERMREFsymbol = 116
  final val TERMREF = 117
  final val TYPEREFsymbol = 118
  final val TYPEREF = 119
  final val SELFDEF = 120

  final val PACKAGE = 128
  final val VALDEF = 129
  final val DEFDEF = 130
  final val TYPEDEF = 131
  final val IMPORT = 132
  final val TYPEPARAM = 133
  final val PARAMS = 134
  final val PARAM = 136
  final val APPLY = 137
  final val TYPEAPPLY = 138
  final val TYPED = 139
  final val NAMEDARG = 140
  final val ASSIGN = 141
  final val BLOCK = 142
  final val IF = 143
  final val LAMBDA = 144
  final val MATCH = 145
  final val RETURN = 146
  final val TRY = 147
  final val INLINED = 148
  final val REPEATED = 149
  final val BIND = 150
  final val ALTERNATIVE = 151
  final val UNAPPLY = 152
  final val ANNOTATEDtype = 153
  final val ANNOTATEDtpt = 154
  final val CASEDEF = 155
  final val TEMPLATE = 156
  final val SUPER = 157
  final val SUPERtype = 158
  final val REFINEDtype = 159
  final val REFINEDtpt = 160
  final val APPLIEDtype = 161
  final val APPLIEDtpt = 162
  final val TYPEBOUNDS = 163
  final val TYPEBOUNDStpt = 164
  final val TYPEALIAS = 165
  final val ANDtype = 166
  final val ANDtpt = 167
  final val ORtype = 168
  final val ORtpt = 169
  final val METHODtype = 170
  final val POLYtype = 171
  final val TYPELAMBDAtype = 172
  final val LAMBDAtpt = 173
  final val PARAMtype = 174
  final val ANNOTATION = 175

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
       | STABLE
       | ANNOTATION
       | PRIVATEqualified
       | PROTECTEDqualified => true
    case _ => false
  }

  def isTypeTreeTag(tag: Int) = tag match {
    case IDENTtpt
       | SELECTtpt
       | SINGLETONtpt
       | REFINEDtpt
       | APPLIEDtpt
       | LAMBDAtpt
       | TYPEBOUNDStpt
       | ANNOTATEDtpt
       | ANDtpt
       | ORtpt
       | BYNAMEtpt
       | BIND => true
    case _ => false
  }

  def nameTagToString(tag: Int): String = tag match {
    case UTF8 => "UTF8"
    case QUALIFIED => "QUALIFIED"
    case FLATTENED => "FLATTENED"
    case EXPANDED => "EXPANDED"
    case EXPANDPREFIX => "EXPANDPREFIX"
    case TRAITSETTER => "TRAITSETTER"
    case UNIQUE => "UNIQUE"
    case DEFAULTGETTER => "DEFAULTGETTER"
    case VARIANT => "VARIANT"
    case OUTERSELECT => "OUTERSELECT"

    case SUPERACCESSOR => "SUPERACCESSOR"
    case PROTECTEDACCESSOR => "PROTECTEDACCESSOR"
    case PROTECTEDSETTER => "PROTECTEDSETTER"
    case INITIALIZER => "INITIALIZER"
    case SHADOWED => "SHADOWED"
    case AVOIDCLASH => "AVOIDCLASH"
    case DIRECT => "DIRECT"
    case FIELD => "FIELD"
    case EXTMETH => "EXTMETH"
    case OBJECTVAR => "OBJECTVAR"
    case OBJECTCLASS => "OBJECTCLASS"

    case SIGNED => "SIGNED"
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
    case IDENTtpt => "IDENTtpt"
    case SELECT => "SELECT"
    case SELECTtpt => "SELECTtpt"
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
    case TYPED => "TYPED"
    case NAMEDARG => "NAMEDARG"
    case ASSIGN => "ASSIGN"
    case BLOCK => "BLOCK"
    case IF => "IF"
    case LAMBDA => "LAMBDA"
    case MATCH => "MATCH"
    case RETURN => "RETURN"
    case INLINED => "INLINED"
    case TRY => "TRY"
    case REPEATED => "REPEATED"
    case BIND => "BIND"
    case ALTERNATIVE => "ALTERNATIVE"
    case UNAPPLY => "UNAPPLY"
    case ANNOTATEDtype => "ANNOTATEDtype"
    case ANNOTATEDtpt => "ANNOTATEDtpt"
    case CASEDEF => "CASEDEF"
    case IMPLICITarg => "IMPLICITarg"
    case TEMPLATE => "TEMPLATE"
    case SELFDEF => "SELFDEF"
    case THIS => "THIS"
    case QUALTHIS => "QUALTHIS"
    case SUPER => "SUPER"
    case CLASSconst => "CLASSconst"
    case ENUMconst => "ENUMconst"
    case SINGLETONtpt => "SINGLETONtpt"
    case SUPERtype => "SUPERtype"
    case REFINEDtype => "REFINEDtype"
    case REFINEDtpt => "REFINEDtpt"
    case APPLIEDtype => "APPLIEDtype"
    case APPLIEDtpt => "APPLIEDtpt"
    case TYPEBOUNDS => "TYPEBOUNDS"
    case TYPEBOUNDStpt => "TYPEBOUNDStpt"
    case TYPEALIAS => "TYPEALIAS"
    case ANDtype => "ANDtype"
    case ANDtpt => "ANDtpt"
    case ORtype => "ORtype"
    case ORtpt => "ORtpt"
    case BYNAMEtype => "BYNAMEtype"
    case BYNAMEtpt => "BYNAMEtpt"
    case POLYtype => "POLYtype"
    case METHODtype => "METHODtype"
    case TYPELAMBDAtype => "TYPELAMBDAtype"
    case LAMBDAtpt => "LAMBDAtpt"
    case PARAMtype => "PARAMtype"
    case ANNOTATION => "ANNOTATION"
    case PRIVATEqualified => "PRIVATEqualified"
    case PROTECTEDqualified => "PROTECTEDqualified"
  }

  /** @return If non-negative, the number of leading references (represented as nats) of a length/trees entry.
   *          If negative, minus the number of leading non-reference trees.
   */
  def numRefs(tag: Int) = tag match {
    case VALDEF | DEFDEF | TYPEDEF | TYPEPARAM | PARAM | NAMEDARG | RETURN | BIND |
         SELFDEF | REFINEDtype => 1
    case RENAMED | PARAMtype => 2
    case POLYtype | METHODtype | TYPELAMBDAtype => -1
    case _ => 0
  }
}
