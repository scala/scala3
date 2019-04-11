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
                  EXPANDED          Length qualified_NameRef selector_NameRef
                  EXPANDPREFIX      Length qualified_NameRef selector_NameRef

                  UNIQUE            Length separator_NameRef uniqid_Nat underlying_NameRef?
                  DEFAULTGETTER     Length underlying_NameRef index_Nat
                  VARIANT           Length underlying_NameRef variance_Nat      // 0: Contravariant, 1: Covariant

                  SUPERACCESSOR     Length underlying_NameRef
                  INLINEACCESSOR    Length underlying_NameRef
                  OBJECTCLASS       Length underlying_NameRef

                  SIGNED            Length original_NameRef resultSig_NameRef paramSig_NameRef*

  NameRef       = Nat                    // ordinal number of name in name table, starting from 1.

Note: Unqualified names in the name table are strings. The context decides whether a name is
a type-name or a term-name. The same string can represent both.

Standard-Section: "ASTs" TopLevelStat*

  TopLevelStat  = PACKAGE        Length Path TopLevelStat*
                  Stat

  Stat          = Term
                  ValOrDefDef
                  TYPEDEF        Length NameRef (type_Term | Template) Modifier*
                  OBJECTDEF      Length NameRef Template Modifier*
                  IMPORT         Length [IMPLIED] qual_Term Selector*
  ValOrDefDef   = VALDEF         Length NameRef type_Term rhs_Term? Modifier*
                  DEFDEF         Length NameRef TypeParam* Params* returnType_Term rhs_Term?
                                        Modifier*
  Selector      = IMPORTED              name_NameRef
                  RENAMED               to_NameRef

                                 // Imports are for scala.meta, they are not used in the backend

  TypeParam     = TYPEPARAM      Length NameRef type_Term Modifier*
  Params        = PARAMS         Length Param*
  Param         = PARAM          Length NameRef type_Term rhs_Term? Modifier*  // rhs_Term is present in the case of an aliased class parameter
  Template      = TEMPLATE       Length TypeParam* Param* parent_Term* Self? Stat* // Stat* always starts with the primary constructor.
  Self          = SELFDEF               selfName_NameRef selfType_Term

  Term          = Path
                  IDENT                 NameRef Type     // used when term ident’s type is not a TermRef
                  SELECT                possiblySigned_NameRef qual_Term
                  QUALTHIS              typeIdent_Tree
                  NEW                   clsType_Term
                  THROW                 throwableExpr_Term
                  NAMEDARG              paramName_NameRef arg_Term
                  APPLY          Length fn_Term arg_Term*
                  TYPEAPPLY      Length fn_Term arg_Type*
                  SUPER          Length this_Term mixinTypeIdent_Tree?
                  TYPED          Length expr_Term ascriptionType_Tern
                  ASSIGN         Length lhs_Term rhs_Term
                  BLOCK          Length expr_Term Stat*
                  INLINED        Length expr_Term call_Term? ValOrDefDef*
                  LAMBDA         Length meth_Term target_Type?
                  IF             Length [INLINE] cond_Term then_Term else_Term
                  MATCH          Length (IMPLICIT | [INLINE] sel_Term) CaseDef*
                  TRY            Length expr_Term CaseDef* finalizer_Term?
                  RETURN         Length meth_ASTRef expr_Term?
                  WHILE          Length cond_Term body_Term
                  REPEATED       Length elem_Type elem_Term*
                  SELECTouter    Length levels_Nat qual_Term underlying_Type
                  BIND           Length boundName_NameRef patType_Type pat_Term
                  ALTERNATIVE    Length alt_Term*
                  UNAPPLY        Length fun_Term ImplicitArg* pat_Type pat_Term*
                  IDENTtpt              NameRef Type      // used for all type idents
                  SELECTtpt             NameRef qual_Term
                  SINGLETONtpt          ref_Term
                  REFINEDtpt     Length underlying_Term refinement_Stat*
                  APPLIEDtpt     Length tycon_Term arg_Term*
                  LAMBDAtpt      Length TypeParam* body_Term
                  TYPEBOUNDStpt  Length low_Term high_Term?
                  ANNOTATEDtpt   Length underlying_Term fullAnnotation_Term
                  MATCHtpt       Length bound_Term? sel_Term CaseDef*
                  BYNAMEtpt             underlying_Term
                  SHAREDterm            term_ASTRef
                  HOLE           Length idx_Nat arg_Tree*

  CaseDef       = CASEDEF        Length pat_Term rhs_Tree guard_Tree?
  ImplicitArg   = IMPLICITARG           arg_Term
  ASTRef        = Nat                               // byte position in AST payload

  Path          = Constant
                  TERMREFdirect         sym_ASTRef
                  TERMREFsymbol         sym_ASTRef qual_Type
                  TERMREFpkg            fullyQualified_NameRef
                  TERMREFin      Length possiblySigned_NameRef qual_Type namespace_Type
                  TERMREF               possiblySigned_NameRef qual_Type
                  THIS                  clsRef_Type
                  RECthis               recType_ASTRef
                  SHAREDtype            path_ASTRef

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
                  SYMBOLconst           NameRef

  Type          = Path
                  TYPEREFdirect         sym_ASTRef
                  TYPEREFsymbol         sym_ASTRef qual_Type
                  TYPEREFpkg            fullyQualified_NameRef
                  TYPEREFin      Length NameRef qual_Type namespace_Type
                  TYPEREF               NameRef qual_Type
                  RECtype               parent_Type
                  TYPEALIAS             alias_Type
                  SUPERtype      Length this_Type underlying_Type
                  REFINEDtype    Length underlying_Type refinement_NameRef info_Type
                  APPLIEDtype    Length tycon_Type arg_Type*
                  TYPEBOUNDS     Length low_Type high_Type
                  ANNOTATEDtype  Length underlying_Type fullAnnotation_Term
                  ANDtype        Length left_Type right_Type
                  ORtype         Length left_Type right_Type
                  MATCHtype      Length bound_Type sel_Type case_Type*
                  BIND           Length boundName_NameRef bounds_Type
                                        // for type-variables defined in a type pattern
                  BYNAMEtype            underlying_Type
                  PARAMtype      Length binder_ASTref paramNum_Nat
                  POLYtype       Length result_Type NamesTypes
                  methodType(_, _) Length result_Type NamesTypes    // needed for refinements
                  TYPELAMBDAtype Length result_Type NamesTypes      // variance encoded in front of name: +/-/(nothing)
                  SHAREDtype            type_ASTRef
  NamesTypes    = NameType*
  NameType      = paramName_NameRef typeOrBounds_ASTRef

  Modifier      = PRIVATE
                  INTERNAL                            // package private
                  PROTECTED
                  PRIVATEqualified     qualifier_Type // to be dropped(?)
                  PROTECTEDqualified   qualifier_Type // to be dropped(?)
                  ABSTRACT
                  FINAL
                  SEALED
                  CASE
                  IMPLICIT
                  IMPLIED
                  ERASED
                  LAZY
                  OVERRIDE
                  OPAQUE
                  INLINE
                  MACRO                               // inline method containing toplevel splices
                  INLINEPROXY                         // symbol of binding representing an inline parameter
                  STATIC                              // mapped to static Java member
                  OBJECT                              // an object or its class
                  TRAIT                               // a trait
                  ENUM                                // a enum class or enum case
                  LOCAL                               // private[this] or protected[this]
                  SYNTHETIC                           // generated by Scala compiler
                  ARTIFACT                            // to be tagged Java Synthetic
                  MUTABLE                             // a var
                  FIELDaccessor                       // getter or setter
                  CASEaccessor                        // getter for case class param
                  COVARIANT                           // type parameter marked “+”
                  CONTRAVARIANT                       // type parameter marked “-”
                  SCALA2X                             // Imported from Scala2.x
                  DEFAULTparameterized                // Method with default parameters
                  STABLE                              // Method that is assumed to be stable
                  EXTENSION                           // An extension method
                  GIVEN                               // new style implicit parameters, introduced with `given`
                  PARAMsetter                         // A setter without a body named `x_=` where `x` is pickled as a PARAM
                  Annotation

  Annotation    = ANNOTATION     Length tycon_Type fullAnnotation_Term

Note: Tree tags are grouped into 5 categories that determine what follows, and thus allow to compute the size of the tagged tree in a generic way.

  Category 1 (tags 1-49)   :  tag
  Category 2 (tags 50-79)  :  tag Nat
  Category 3 (tags 80-109) :  tag AST
  Category 4 (tags 110-127):  tag Nat AST
  Category 5 (tags 128-255):  tag Length <payload>

Standard Section: "Positions" Assoc*

  Assoc         = Header offset_Delta? offset_Delta? point_Delta?
                | SOURCE nameref_Int
  Header        = addr_Delta +              // in one Nat: difference of address to last recorded node << 3 +
                  hasStartDiff +            // one bit indicating whether there follows a start address delta << 2
                  hasEndDiff +              // one bit indicating whether there follows an end address delta << 1
                  hasPoint                  // one bit indicating whether the new position has a point (i.e ^ position)
                                            // Nodes which have the same positions as their parents are omitted.
                                            // offset_Deltas give difference of start/end offset wrt to the
                                            // same offset in the previously recorded node (or 0 for the first recorded node)
  Delta         = Int                       // Difference between consecutive offsets,
  SOURCE        = 4                         // Impossible as header, since addr_Delta = 0 implies that we refer to the
                                            // same tree as the previous one, but then hasStartDiff = 1 implies that
                                            // the tree's range starts later than the range of itself.

All elements of a position section are serialized as Ints

Standard Section: "Comments" Comment*

  Comment       = Length Bytes LongInt      // Raw comment's bytes encoded as UTF-8, followed by the comment's coordinates.


**************************************************************************************/

object TastyFormat {

  final val header: Array[Int] = Array(0x5C, 0xA1, 0xAB, 0x1F)
  val MajorVersion: Int = 13
  val MinorVersion: Int = 0

  /** Tags used to serialize names */
  class NameTags {
    final val UTF8 = 1               // A simple name in UTF8 encoding.

    final val QUALIFIED = 2          // A fully qualified name `<prefix>.<suffix>`.

    final val EXPANDED = 3           // An expanded name `<prefix>$$<suffix>`,
                                     // used by Scala-2 for private names.

    final val EXPANDPREFIX = 4       // An expansion prefix `<prefix>$<suffix>`,
                                     // used by Scala-2 for private names.

    final val UNIQUE = 10            // A unique name `<name>$<num>` where `<num>`
                                     // is used only once for each `<name>`.

    final val DEFAULTGETTER = 11     // The name `<meth-name>$default$<param-num>`
                                     // of a default getter that returns a default argument.

    final val VARIANT = 12           // A name `+<name>` o `-<name>` indicating
                                     // a co- or contra-variant parameter of a type lambda.

    final val SUPERACCESSOR = 20     // The name of a super accessor `super$name` created by SuperAccesors.

    final val INLINEACCESSOR = 21    // The name of an inline accessor `inline$name`

    final val OBJECTCLASS = 23       // The name of an object class (or: module class) `<name>$`.

    final val SIGNED = 63            // A pair of a name and a signature, used to identify
                                     // possibly overloaded methods.
  }
  object NameTags extends NameTags

  // Position header

  final val SOURCE = 4

 // AST tags
  // Cat. 1:    tag

  final val firstSimpleTreeTag = UNITconst
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
  final val INLINEPROXY = 16
  final val INLINE = 17
  final val STATIC = 18
  final val OBJECT = 19
  final val TRAIT = 20
  final val ENUM = 21
  final val LOCAL = 22
  final val SYNTHETIC = 23
  final val ARTIFACT = 24
  final val MUTABLE = 25
  final val FIELDaccessor = 26
  final val CASEaccessor = 27
  final val COVARIANT = 28
  final val CONTRAVARIANT = 29
  final val SCALA2X = 30
  final val DEFAULTparameterized = 31
  final val STABLE = 32
  final val MACRO = 33
  final val ERASED = 34
  final val OPAQUE = 35
  final val EXTENSION = 36
  final val GIVEN = 37
  final val IMPLIED = 38
  final val PARAMsetter = 39

  // Cat. 2:    tag Nat

  final val SHAREDterm = 50
  final val SHAREDtype = 51
  final val TERMREFdirect = 52
  final val TYPEREFdirect = 53
  final val TERMREFpkg = 54
  final val TYPEREFpkg = 55
  final val RECthis = 56
  final val BYTEconst = 57
  final val SHORTconst = 58
  final val CHARconst = 59
  final val INTconst = 60
  final val LONGconst = 61
  final val FLOATconst = 62
  final val DOUBLEconst = 63
  final val STRINGconst = 64
  final val IMPORTED = 65
  final val RENAMED = 66
  final val SYMBOLconst = 67

  // Cat. 3:    tag AST

  final val THIS = 80
  final val QUALTHIS = 81
  final val CLASSconst = 82
  final val ENUMconst = 83
  final val BYNAMEtype = 84
  final val BYNAMEtpt = 85
  final val NEW = 86
  final val THROW = 87
  final val IMPLICITarg = 88
  final val PRIVATEqualified = 89
  final val PROTECTEDqualified = 90
  final val RECtype = 91
  final val TYPEALIAS = 92
  final val SINGLETONtpt = 93

  // Cat. 4:    tag Nat AST

  final val IDENT = 110
  final val IDENTtpt = 111
  final val SELECT = 112
  final val SELECTtpt = 113
  final val TERMREFsymbol = 114
  final val TERMREF = 115
  final val TYPEREFsymbol = 116
  final val TYPEREF = 117
  final val SELFDEF = 118
  final val NAMEDARG = 119

  // Cat. 5:    tag Length ...

  final val PACKAGE = 128
  final val VALDEF = 129
  final val DEFDEF = 130
  final val TYPEDEF = 131
  final val IMPORT = 132
  final val TYPEPARAM = 133
  final val PARAMS = 134
  final val PARAM = 135
  final val APPLY = 136
  final val TYPEAPPLY = 137
  final val TYPED = 138
  final val ASSIGN = 139
  final val BLOCK = 140
  final val IF = 141
  final val LAMBDA = 142
  final val MATCH = 143
  final val RETURN = 144
  final val WHILE = 145
  final val TRY = 146
  final val INLINED = 147
  final val SELECTouter = 148
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
  final val ANDtype = 165
  final val ORtype = 167
  final val POLYtype = 169
  final val TYPELAMBDAtype = 170
  final val LAMBDAtpt = 171
  final val PARAMtype = 172
  final val ANNOTATION = 173
  final val TERMREFin = 174
  final val TYPEREFin = 175
  final val OBJECTDEF = 176

  final val METHODtype = 180
  final val ERASEDMETHODtype = 181
  final val CONTEXTUALMETHODtype = 182
  final val ERASEDCONTEXTUALMETHODtype = 183
  final val IMPLICITMETHODtype = 184

  final val MATCHtype = 190
  final val MATCHtpt = 191

  def methodType(isContextual: Boolean, isImplicit: Boolean, isErased: Boolean): Int = {
    val implicitOffset =
      if (isContextual) 2
      else if (isImplicit) { assert(!isErased); 4 }
      else 0
    val erasedOffset = if (isErased) 1 else 0
    METHODtype + erasedOffset + implicitOffset
  }

  final val HOLE = 255

  final val firstNatTreeTag = SHAREDterm
  final val firstASTTreeTag = THIS
  final val firstNatASTTreeTag = IDENT
  final val firstLengthTreeTag = PACKAGE

  /** Useful for debugging */
  def isLegalTag(tag: Int): Boolean =
    firstSimpleTreeTag <= tag && tag <= PARAMsetter ||
    firstNatTreeTag <= tag && tag <= SYMBOLconst ||
    firstASTTreeTag <= tag && tag <= SINGLETONtpt ||
    firstNatASTTreeTag <= tag && tag <= NAMEDARG ||
    firstLengthTreeTag <= tag && tag <= MATCHtpt ||
    tag == HOLE

  def isParamTag(tag: Int): Boolean = tag == PARAM || tag == TYPEPARAM

  def isModifierTag(tag: Int): Boolean = tag match {
    case PRIVATE
       | INTERNAL
       | PROTECTED
       | ABSTRACT
       | FINAL
       | SEALED
       | CASE
       | IMPLICIT
       | IMPLIED
       | ERASED
       | LAZY
       | OVERRIDE
       | INLINE
       | INLINEPROXY
       | MACRO
       | OPAQUE
       | STATIC
       | OBJECT
       | TRAIT
       | LOCAL
       | SYNTHETIC
       | ARTIFACT
       | MUTABLE
       | FIELDaccessor
       | CASEaccessor
       | COVARIANT
       | CONTRAVARIANT
       | SCALA2X
       | DEFAULTparameterized
       | STABLE
       | EXTENSION
       | GIVEN
       | PARAMsetter
       | ANNOTATION
       | PRIVATEqualified
       | PROTECTEDqualified => true
    case _ => false
  }

  def isTypeTreeTag(tag: Int): Boolean = tag match {
    case IDENTtpt
       | SELECTtpt
       | SINGLETONtpt
       | REFINEDtpt
       | APPLIEDtpt
       | LAMBDAtpt
       | TYPEBOUNDStpt
       | ANNOTATEDtpt
       | BYNAMEtpt
       | MATCHtpt
       | BIND => true
    case _ => false
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
    case IMPLIED => "IMPLIED"
    case ERASED => "ERASED"
    case LAZY => "LAZY"
    case OVERRIDE => "OVERRIDE"
    case INLINE => "INLINE"
    case INLINEPROXY => "INLINEPROXY"
    case MACRO => "MACRO"
    case OPAQUE => "OPAQUE"
    case STATIC => "STATIC"
    case OBJECT => "OBJECT"
    case TRAIT => "TRAIT"
    case ENUM => "ENUM"
    case LOCAL => "LOCAL"
    case SYNTHETIC => "SYNTHETIC"
    case ARTIFACT => "ARTIFACT"
    case MUTABLE => "MUTABLE"
    case FIELDaccessor => "FIELDaccessor"
    case CASEaccessor => "CASEaccessor"
    case COVARIANT => "COVARIANT"
    case CONTRAVARIANT => "CONTRAVARIANT"
    case SCALA2X => "SCALA2X"
    case DEFAULTparameterized => "DEFAULTparameterized"
    case STABLE => "STABLE"
    case EXTENSION => "EXTENSION"
    case GIVEN => "GIVEN"
    case PARAMsetter => "PARAMsetter"

    case SHAREDterm => "SHAREDterm"
    case SHAREDtype => "SHAREDtype"
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
    case OBJECTDEF => "OBJECTDEF"
    case IMPORT => "IMPORT"
    case TYPEPARAM => "TYPEPARAM"
    case PARAMS => "PARAMS"
    case PARAM => "PARAM"
    case IMPORTED => "IMPORTED"
    case RENAMED => "RENAMED"
    case APPLY => "APPLY"
    case TYPEAPPLY => "TYPEAPPLY"
    case NEW => "NEW"
    case THROW => "THROW"
    case TYPED => "TYPED"
    case NAMEDARG => "NAMEDARG"
    case ASSIGN => "ASSIGN"
    case BLOCK => "BLOCK"
    case IF => "IF"
    case LAMBDA => "LAMBDA"
    case MATCH => "MATCH"
    case RETURN => "RETURN"
    case WHILE => "WHILE"
    case INLINED => "INLINED"
    case SELECTouter => "SELECTouter"
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
    case SYMBOLconst => "SYMBOLconst"
    case SINGLETONtpt => "SINGLETONtpt"
    case SUPERtype => "SUPERtype"
    case TERMREFin => "TERMREFin"
    case TYPEREFin => "TYPEREFin"

    case REFINEDtype => "REFINEDtype"
    case REFINEDtpt => "REFINEDtpt"
    case APPLIEDtype => "APPLIEDtype"
    case APPLIEDtpt => "APPLIEDtpt"
    case TYPEBOUNDS => "TYPEBOUNDS"
    case TYPEBOUNDStpt => "TYPEBOUNDStpt"
    case TYPEALIAS => "TYPEALIAS"
    case ANDtype => "ANDtype"
    case ORtype => "ORtype"
    case BYNAMEtype => "BYNAMEtype"
    case BYNAMEtpt => "BYNAMEtpt"
    case POLYtype => "POLYtype"
    case METHODtype => "METHODtype"
    case ERASEDMETHODtype => "ERASEDMETHODtype"
    case CONTEXTUALMETHODtype => "CONTEXTUALMETHODtype"
    case ERASEDCONTEXTUALMETHODtype => "ERASEDCONTEXTUALMETHODtype"
    case IMPLICITMETHODtype => "IMPLICITMETHODtype"
    case TYPELAMBDAtype => "TYPELAMBDAtype"
    case LAMBDAtpt => "LAMBDAtpt"
    case MATCHtype => "MATCHtype"
    case MATCHtpt => "MATCHtpt"
    case PARAMtype => "PARAMtype"
    case ANNOTATION => "ANNOTATION"
    case PRIVATEqualified => "PRIVATEqualified"
    case PROTECTEDqualified => "PROTECTEDqualified"
    case HOLE => "HOLE"
  }

  /** @return If non-negative, the number of leading references (represented as nats) of a length/trees entry.
   *          If negative, minus the number of leading non-reference trees.
   */
  def numRefs(tag: Int): Int = tag match {
    case VALDEF | DEFDEF | TYPEDEF | OBJECTDEF | TYPEPARAM | PARAM | NAMEDARG | RETURN | BIND |
         SELFDEF | REFINEDtype | TERMREFin | TYPEREFin | HOLE => 1
    case RENAMED | PARAMtype => 2
    case POLYtype | TYPELAMBDAtype |
         METHODtype | ERASEDMETHODtype |
         CONTEXTUALMETHODtype | ERASEDCONTEXTUALMETHODtype |
         IMPLICITMETHODtype => -1
    case _ => 0
  }
}
