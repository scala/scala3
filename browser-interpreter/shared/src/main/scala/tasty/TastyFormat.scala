package tasty

/**
 * Cross-platform TASTy format constants.
 * Adapted from dotty.tools.tasty.TastyFormat for Scala.js compatibility.
 */
object TastyFormat {

  /** The first four bytes of a TASTy file */
  final val header: Array[Int] = Array(0x5C, 0xA1, 0xAB, 0x1F)

  /** TASTy major version - breaking backward compatibility */
  final val MajorVersion: Int = 28

  /** TASTy minor version - breaking forward compatibility */
  final val MinorVersion: Int = 8

  /** TASTy experimental version - 0 for stable releases */
  final val ExperimentalVersion: Int = 1

  /** Check version compatibility */
  def isVersionCompatible(
    fileMajor: Int,
    fileMinor: Int,
    fileExperimental: Int,
    compilerMajor: Int,
    compilerMinor: Int,
    compilerExperimental: Int
  ): Boolean = (
    fileMajor == compilerMajor &&
      (  fileMinor == compilerMinor && fileExperimental == compilerExperimental
      || fileMinor <  compilerMinor && fileExperimental == 0
    )
  )

  final val ASTsSection = "ASTs"
  final val PositionsSection = "Positions"
  final val CommentsSection = "Comments"
  final val AttributesSection = "Attributes"

  // Name tags
  object NameTags {
    final val UTF8 = 1
    final val QUALIFIED = 2
    final val EXPANDED = 3
    final val EXPANDPREFIX = 4
    final val UNIQUE = 10
    final val DEFAULTGETTER = 11
    final val SUPERACCESSOR = 20
    final val INLINEACCESSOR = 21
    final val BODYRETAINER = 22
    final val OBJECTCLASS = 23
    final val SIGNED = 63
    final val TARGETSIGNED = 62
  }

  // Position header
  final val SOURCE = 4

  // AST tags - Tree Cat. 1: tag
  final val firstSimpleTreeTag = UNITconst
  final val UNITconst = 2
  final val FALSEconst = 3
  final val TRUEconst = 4
  final val NULLconst = 5
  final val PRIVATE = 6
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
  final val HASDEFAULT = 31
  final val STABLE = 32
  final val MACRO = 33
  final val ERASED = 34
  final val OPAQUE = 35
  final val EXTENSION = 36
  final val GIVEN = 37
  final val PARAMsetter = 38
  final val EXPORTED = 39
  final val OPEN = 40
  final val PARAMalias = 41
  final val TRANSPARENT = 42
  final val INFIX = 43
  final val INVISIBLE = 44
  final val EMPTYCLAUSE = 45
  final val SPLITCLAUSE = 46
  final val TRACKED = 47
  final val SUBMATCH = 48
  final val INTO = 49

  // Tree Cat. 2: tag Nat
  final val firstNatTreeTag = SHAREDterm
  final val SHAREDterm = 60
  final val SHAREDtype = 61
  final val TERMREFdirect = 62
  final val TYPEREFdirect = 63
  final val TERMREFpkg = 64
  final val TYPEREFpkg = 65
  final val RECthis = 66
  final val BYTEconst = 67
  final val SHORTconst = 68
  final val CHARconst = 69
  final val INTconst = 70
  final val LONGconst = 71
  final val FLOATconst = 72
  final val DOUBLEconst = 73
  final val STRINGconst = 74
  final val IMPORTED = 75
  final val RENAMED = 76

  // Tree Cat. 3: tag AST
  final val firstASTTreeTag = THIS
  final val THIS = 90
  final val QUALTHIS = 91
  final val CLASSconst = 92
  final val BYNAMEtype = 93
  final val BYNAMEtpt = 94
  final val NEW = 95
  final val THROW = 96
  final val IMPLICITarg = 97
  final val PRIVATEqualified = 98
  final val PROTECTEDqualified = 99
  final val RECtype = 100
  final val SINGLETONtpt = 101
  final val BOUNDED = 102
  final val EXPLICITtpt = 103
  final val ELIDED = 104

  // Tree Cat. 4: tag Nat AST
  final val firstNatASTTreeTag = IDENT
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

  // Tree Cat. 5: tag Length ...
  final val firstLengthTreeTag = PACKAGE
  final val PACKAGE = 128
  final val VALDEF = 129
  final val DEFDEF = 130
  final val TYPEDEF = 131
  final val IMPORT = 132
  final val TYPEPARAM = 133
  final val PARAM = 134
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
  final val SELECTin = 176
  final val EXPORT = 177
  final val QUOTE = 178
  final val SPLICE = 179
  final val METHODtype = 180
  final val APPLYsigpoly = 181
  final val QUOTEPATTERN = 182
  final val SPLICEPATTERN = 183
  final val MATCHtype = 190
  final val MATCHtpt = 191
  final val MATCHCASEtype = 192
  final val FLEXIBLEtype = 193
  final val HOLE = 255

  // Attribute tags
  def isBooleanAttrTag(tag: Int): Boolean = 1 <= tag && tag <= 32
  final val SCALA2STANDARDLIBRARYattr = 1
  final val EXPLICITNULLSattr = 2
  final val CAPTURECHECKEDattr = 3
  final val WITHPUREFUNSattr = 4
  final val JAVAattr = 5
  final val OUTLINEattr = 6

  def isStringAttrTag(tag: Int): Boolean = 129 <= tag && tag <= 160
  final val SOURCEFILEattr = 129

  /** Useful for debugging */
  def isLegalTag(tag: Int): Boolean =
    firstSimpleTreeTag <= tag && tag <= SPLITCLAUSE ||
    firstNatTreeTag <= tag && tag <= RENAMED ||
    firstASTTreeTag <= tag && tag <= BOUNDED ||
    firstNatASTTreeTag <= tag && tag <= NAMEDARG ||
    firstLengthTreeTag <= tag && tag <= FLEXIBLEtype ||
    tag == HOLE

  def isParamTag(tag: Int): Boolean = tag == PARAM || tag == TYPEPARAM

  def isModifierTag(tag: Int): Boolean = tag match {
    case PRIVATE | PROTECTED | ABSTRACT | FINAL | SEALED | CASE | IMPLICIT |
         GIVEN | ERASED | LAZY | OVERRIDE | INLINE | INLINEPROXY | MACRO |
         OPAQUE | STATIC | OBJECT | TRAIT | TRANSPARENT | INFIX | ENUM |
         LOCAL | SYNTHETIC | ARTIFACT | MUTABLE | FIELDaccessor | CASEaccessor |
         COVARIANT | CONTRAVARIANT | HASDEFAULT | STABLE | EXTENSION |
         PARAMsetter | PARAMalias | EXPORTED | OPEN | INVISIBLE |
         ANNOTATION | PRIVATEqualified | PROTECTEDqualified | TRACKED | INTO => true
    case _ => false
  }

  def isTypeTreeTag(tag: Int): Boolean = tag match {
    case IDENTtpt | SELECTtpt | SINGLETONtpt | REFINEDtpt | APPLIEDtpt |
         LAMBDAtpt | TYPEBOUNDStpt | ANNOTATEDtpt | BYNAMEtpt | MATCHtpt |
         EXPLICITtpt | BIND => true
    case _ => false
  }

  def astTagToString(tag: Int): String = tag match {
    case UNITconst => "UNITconst"
    case FALSEconst => "FALSEconst"
    case TRUEconst => "TRUEconst"
    case NULLconst => "NULLconst"
    case PRIVATE => "PRIVATE"
    case PROTECTED => "PROTECTED"
    case ABSTRACT => "ABSTRACT"
    case FINAL => "FINAL"
    case SEALED => "SEALED"
    case CASE => "CASE"
    case IMPLICIT => "IMPLICIT"
    case LAZY => "LAZY"
    case OVERRIDE => "OVERRIDE"
    case INLINE => "INLINE"
    case OBJECT => "OBJECT"
    case TRAIT => "TRAIT"
    case ENUM => "ENUM"
    case SYNTHETIC => "SYNTHETIC"
    case MUTABLE => "MUTABLE"
    case STABLE => "STABLE"
    case GIVEN => "GIVEN"
    case OPEN => "OPEN"

    case SHAREDterm => "SHAREDterm"
    case SHAREDtype => "SHAREDtype"
    case TERMREFdirect => "TERMREFdirect"
    case TYPEREFdirect => "TYPEREFdirect"
    case TERMREFpkg => "TERMREFpkg"
    case TYPEREFpkg => "TYPEREFpkg"
    case BYTEconst => "BYTEconst"
    case SHORTconst => "SHORTconst"
    case CHARconst => "CHARconst"
    case INTconst => "INTconst"
    case LONGconst => "LONGconst"
    case FLOATconst => "FLOATconst"
    case DOUBLEconst => "DOUBLEconst"
    case STRINGconst => "STRINGconst"

    case THIS => "THIS"
    case NEW => "NEW"
    case THROW => "THROW"

    case IDENT => "IDENT"
    case IDENTtpt => "IDENTtpt"
    case SELECT => "SELECT"
    case SELECTtpt => "SELECTtpt"
    case TERMREF => "TERMREF"
    case TYPEREF => "TYPEREF"
    case NAMEDARG => "NAMEDARG"

    case PACKAGE => "PACKAGE"
    case VALDEF => "VALDEF"
    case DEFDEF => "DEFDEF"
    case TYPEDEF => "TYPEDEF"
    case IMPORT => "IMPORT"
    case TYPEPARAM => "TYPEPARAM"
    case PARAM => "PARAM"
    case APPLY => "APPLY"
    case TYPEAPPLY => "TYPEAPPLY"
    case TYPED => "TYPED"
    case ASSIGN => "ASSIGN"
    case BLOCK => "BLOCK"
    case IF => "IF"
    case LAMBDA => "LAMBDA"
    case MATCH => "MATCH"
    case RETURN => "RETURN"
    case WHILE => "WHILE"
    case TRY => "TRY"
    case INLINED => "INLINED"
    case REPEATED => "REPEATED"
    case BIND => "BIND"
    case ALTERNATIVE => "ALTERNATIVE"
    case UNAPPLY => "UNAPPLY"
    case CASEDEF => "CASEDEF"
    case TEMPLATE => "TEMPLATE"
    case SUPER => "SUPER"
    case ANNOTATION => "ANNOTATION"

    case _ => s"TAG($tag)"
  }

  /** Number of leading references in length/trees entry */
  def numRefs(tag: Int): Int = tag match {
    case VALDEF | DEFDEF | TYPEDEF | TYPEPARAM | PARAM | NAMEDARG | RETURN | BIND |
         SELFDEF | REFINEDtype | TERMREFin | TYPEREFin | SELECTin | HOLE => 1
    case RENAMED | PARAMtype => 2
    case POLYtype | TYPELAMBDAtype | METHODtype => -1
    case _ => 0
  }
}

