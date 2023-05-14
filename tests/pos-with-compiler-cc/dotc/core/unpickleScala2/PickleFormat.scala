package dotty.tools
package dotc
package core
package unpickleScala2

/** This object provides constants for pickling attributes.
 *
 *  If you extend the format, be sure to increase the
 *  version minor number.
 *
 *  This was adapted from https://github.com/scala/scala/blob/2.11.x/src/reflect/scala/reflect/internal/pickling/PickleFormat.scala
 *
 *  @author Martin Odersky
 *  @version 1.0
 */
object PickleFormat {

/***************************************************
 * Symbol table attribute format:
 *   Symtab         = nentries_Nat {Entry}
 *   Entry          = 1 TERMNAME len_Nat NameInfo
 *                  | 2 TYPENAME len_Nat NameInfo
 *                  | 3 NONEsym len_Nat
 *                  | 4 TYPEsym len_Nat SymbolInfo
 *                  | 5 ALIASsym len_Nat SymbolInfo
 *                  | 6 CLASSsym len_Nat SymbolInfo [thistype_Ref]
 *                  | 7 MODULEsym len_Nat SymbolInfo
 *                  | 8 VALsym len_Nat [defaultGetter_Ref /* no longer needed*/] SymbolInfo [alias_Ref]
 *                  | 9 EXTref len_Nat name_Ref [owner_Ref]
 *                  | 10 EXTMODCLASSref len_Nat name_Ref [owner_Ref]
 *                  | 11 NOtpe len_Nat
 *                  | 12 NOPREFIXtpe len_Nat
 *                  | 13 THIStpe len_Nat sym_Ref
 *                  | 14 SINGLEtpe len_Nat type_Ref sym_Ref
 *                  | 15 CONSTANTtpe len_Nat constant_Ref
 *                  | 16 TYPEREFtpe len_Nat type_Ref sym_Ref {targ_Ref}
 *                  | 17 TYPEBOUNDStpe len_Nat tpe_Ref tpe_Ref
 *                  | 18 REFINEDtpe len_Nat classsym_Ref {tpe_Ref}
 *                  | 19 CLASSINFOtpe len_Nat classsym_Ref {tpe_Ref}
 *                  | 20 METHODtpe len_Nat tpe_Ref {sym_Ref}
 *                  | 21 POLYTtpe len_Nat tpe_Ref {sym_Ref}
 *                  | 22 IMPLICITMETHODtpe len_Nat tpe_Ref {sym_Ref} /* no longer needed */
 *                  | 52 SUPERtpe len_Nat tpe_Ref tpe_Ref
 *                  | 24 LITERALunit len_Nat
 *                  | 25 LITERALboolean len_Nat value_Long
 *                  | 26 LITERALbyte len_Nat value_Long
 *                  | 27 LITERALshort len_Nat value_Long
 *                  | 28 LITERALchar len_Nat value_Long
 *                  | 29 LITERALint len_Nat value_Long
 *                  | 30 LITERALlong len_Nat value_Long
 *                  | 31 LITERALfloat len_Nat value_Long
 *                  | 32 LITERALdouble len_Nat value_Long
 *                  | 33 LITERALstring len_Nat name_Ref
 *                  | 34 LITERALnull len_Nat
 *                  | 35 LITERALclass len_Nat tpe_Ref
 *                  | 36 LITERALenum len_Nat sym_Ref
 *                  | 40 SYMANNOT len_Nat sym_Ref AnnotInfoBody
 *                  | 41 CHILDREN len_Nat sym_Ref {sym_Ref}
 *                  | 42 ANNOTATEDtpe len_Nat [sym_Ref /* no longer needed */] tpe_Ref {annotinfo_Ref}
 *                  | 43 ANNOTINFO len_Nat AnnotInfoBody
 *                  | 44 ANNOTARGARRAY len_Nat {constAnnotArg_Ref}
 *                  | 47 DEBRUIJNINDEXtpe len_Nat level_Nat index_Nat /* no longer needed */
 *                  | 48 EXISTENTIALtpe len_Nat type_Ref {symbol_Ref}
 *                  | 49 TREE len_Nat 1 EMPTYtree
 *                  | 49 TREE len_Nat 2 PACKAGEtree type_Ref sym_Ref mods_Ref name_Ref {tree_Ref}
 *                  | 49 TREE len_Nat 3 CLASStree type_Ref sym_Ref mods_Ref name_Ref tree_Ref {tree_Ref}
 *                  | 49 TREE len_Nat 4 MODULEtree type_Ref sym_Ref mods_Ref name_Ref tree_Ref
 *                  | 49 TREE len_Nat 5 VALDEFtree type_Ref sym_Ref mods_Ref name_Ref tree_Ref tree_Ref
 *                  | 49 TREE len_Nat 6 DEFDEFtree type_Ref sym_Ref mods_Ref name_Ref numtparams_Nat {tree_Ref} numparamss_Nat {numparams_Nat {tree_Ref}} tree_Ref tree_Ref
 *                  | 49 TREE len_Nat 7 TYPEDEFtree type_Ref sym_Ref mods_Ref name_Ref tree_Ref {tree_Ref}
 *                  | 49 TREE len_Nat 8 LABELtree type_Ref sym_Ref tree_Ref {tree_Ref}
 *                  | 49 TREE len_Nat 9 IMPORTtree type_Ref sym_Ref tree_Ref {name_Ref name_Ref}
 *                  | 49 TREE len_Nat 11 DOCDEFtree type_Ref sym_Ref string_Ref tree_Ref
 *                  | 49 TREE len_Nat 12 TEMPLATEtree type_Ref sym_Ref numparents_Nat {tree_Ref} tree_Ref {tree_Ref}
 *                  | 49 TREE len_Nat 13 BLOCKtree type_Ref tree_Ref {tree_Ref}
 *                  | 49 TREE len_Nat 14 CASEtree type_Ref tree_Ref tree_Ref tree_Ref
 *                  | 49 TREE len_Nat 15 SEQUENCEtree type_Ref {tree_Ref}
 *                  | 49 TREE len_Nat 16 ALTERNATIVEtree type_Ref {tree_Ref}
 *                  | 49 TREE len_Nat 17 STARtree type_Ref {tree_Ref}
 *                  | 49 TREE len_Nat 18 BINDtree type_Ref sym_Ref name_Ref tree_Ref
 *                  | 49 TREE len_Nat 19 UNAPPLYtree type_Ref tree_Ref {tree_Ref}
 *                  | 49 TREE len_Nat 20 ARRAYVALUEtree type_Ref tree_Ref {tree_Ref}
 *                  | 49 TREE len_Nat 21 FUNCTIONtree type_Ref sym_Ref tree_Ref {tree_Ref}
 *                  | 49 TREE len_Nat 22 ASSIGNtree type_Ref tree_Ref tree_Ref
 *                  | 49 TREE len_Nat 23 IFtree type_Ref tree_Ref tree_Ref tree_Ref
 *                  | 49 TREE len_Nat 24 MATCHtree type_Ref tree_Ref {tree_Ref}
 *                  | 49 TREE len_Nat 25 RETURNtree type_Ref sym_Ref tree_Ref
 *                  | 49 TREE len_Nat 26 TREtree type_Ref tree_Ref tree_Ref {tree_Ref}
 *                  | 49 TREE len_Nat 27 THROWtree type_Ref tree_Ref
 *                  | 49 TREE len_Nat 28 NEWtree type_Ref tree_Ref
 *                  | 49 TREE len_Nat 29 TYPEDtree type_Ref tree_Ref tree_Ref
 *                  | 49 TREE len_Nat 30 TYPEAPPLYtree type_Ref tree_Ref {tree_Ref}
 *                  | 49 TREE len_Nat 31 APPLYtree type_Ref tree_Ref {tree_Ref}
 *                  | 49 TREE len_Nat 32 APPLYDYNAMICtree type_Ref sym_Ref tree_Ref {tree_Ref}
 *                  | 49 TREE len_Nat 33 SUPERtree type_Ref sym_Ref tree_Ref name_Ref
 *                  | 49 TREE len_Nat 34 THIStree type_Ref sym_Ref  name_Ref
 *                  | 49 TREE len_Nat 35 SELECTtree type_Ref sym_Ref tree_Ref name_Ref
 *                  | 49 TREE len_Nat 36 IDENTtree type_Ref sym_Ref name_Ref
 *                  | 49 TREE len_Nat 37 LITERALtree type_Ref constant_Ref
 *                  | 49 TREE len_Nat 38 TYPEtree type_Ref
 *                  | 49 TREE len_Nat 39 ANNOTATEDtree type_Ref tree_Ref tree_Ref
 *                  | 49 TREE len_Nat 40 SINGLETONTYPEtree type_Ref tree_Ref
 *                  | 49 TREE len_Nat 41 SELECTFROMTYPEtree type_Ref tree_Ref name_Ref
 *                  | 49 TREE len_Nat 42 COMPOUNDTYPEtree type_Ref tree_Ref
 *                  | 49 TREE len_Nat 43 APPLIEDTYPEtree type_Ref tree_Ref {tree_Ref}
 *                  | 49 TREE len_Nat 44 TYPEBOUNDStree type_Ref tree_Ref tree_Ref
 *                  | 49 TREE len_Nat 45 EXISTENTIALTYPEtree type_Ref tree_Ref {tree_Ref}
 *                  | 50 MODIFIERS len_Nat flags_Long privateWithin_Ref
 *   SymbolInfo     = name_Ref owner_Ref flags_LongNat [privateWithin_Ref] info_Ref
 *   NameInfo       = <character sequence of length len_Nat in Utf8 format>
 *   NumInfo        = <len_Nat-byte signed number in big endian format>
 *   Ref            = Nat
 *   AnnotInfoBody  = info_Ref {annotArg_Ref} {name_Ref constAnnotArg_Ref}
 *   AnnotArg       = Tree | Constant
 *   ConstAnnotArg  = Constant | AnnotInfo | AnnotArgArray
 *
 *   len is remaining length after `len`.
 */
  val MajorVersion: Int = 5
  val MinorVersion: Int = 2

  inline val TERMname = 1
  inline val TYPEname = 2
  inline val NONEsym = 3
  inline val TYPEsym = 4
  inline val ALIASsym = 5
  inline val CLASSsym = 6
  inline val MODULEsym = 7
  inline val VALsym = 8
  inline val EXTref = 9
  inline val EXTMODCLASSref = 10
  inline val NOtpe = 11
  inline val NOPREFIXtpe = 12
  inline val THIStpe = 13
  inline val SINGLEtpe = 14
  inline val CONSTANTtpe = 15
  inline val TYPEREFtpe = 16
  inline val TYPEBOUNDStpe = 17
  inline val REFINEDtpe = 18
  inline val CLASSINFOtpe = 19
  inline val METHODtpe = 20
  inline val POLYtpe = 21
  inline val IMPLICITMETHODtpe = 22    // no longer generated

  inline val LITERAL = 23   // base line for literals
  inline val LITERALunit = 24
  inline val LITERALboolean = 25
  inline val LITERALbyte = 26
  inline val LITERALshort = 27
  inline val LITERALchar = 28
  inline val LITERALint = 29
  inline val LITERALlong = 30
  inline val LITERALfloat = 31
  inline val LITERALdouble = 32
  inline val LITERALstring = 33
  inline val LITERALnull = 34
  inline val LITERALclass = 35
  inline val LITERALenum = 36
  inline val SYMANNOT = 40
  inline val CHILDREN = 41
  inline val ANNOTATEDtpe = 42
  inline val ANNOTINFO = 43
  inline val ANNOTARGARRAY = 44

  inline val SUPERtpe = 46
  inline val DEBRUIJNINDEXtpe = 47   // no longer generated
  inline val EXISTENTIALtpe = 48

  inline val TREE = 49      // prefix code that means a tree is coming
    inline val EMPTYtree = 1
    inline val PACKAGEtree = 2
    inline val CLASStree = 3
    inline val MODULEtree = 4
    inline val VALDEFtree = 5
    inline val DEFDEFtree = 6
    inline val TYPEDEFtree = 7
    inline val LABELtree = 8
    inline val IMPORTtree = 9
    inline val DOCDEFtree = 11
    inline val TEMPLATEtree = 12
    inline val BLOCKtree = 13
    inline val CASEtree = 14
    // This node type has been removed.
    // inline val SEQUENCEtree = 15
    inline val ALTERNATIVEtree = 16
    inline val STARtree = 17
    inline val BINDtree = 18
    inline val UNAPPLYtree = 19
    inline val ARRAYVALUEtree = 20
    inline val FUNCTIONtree = 21
    inline val ASSIGNtree = 22
    inline val IFtree = 23
    inline val MATCHtree = 24
    inline val RETURNtree = 25
    inline val TREtree = 26
    inline val THROWtree = 27
    inline val NEWtree = 28
    inline val TYPEDtree = 29
    inline val TYPEAPPLYtree = 30
    inline val APPLYtree = 31
    inline val APPLYDYNAMICtree = 32
    inline val SUPERtree = 33
    inline val THIStree = 34
    inline val SELECTtree = 35
    inline val IDENTtree = 36
    inline val LITERALtree = 37
    inline val TYPEtree = 38
    inline val ANNOTATEDtree = 39
    inline val SINGLETONTYPEtree = 40
    inline val SELECTFROMTYPEtree = 41
    inline val COMPOUNDTYPEtree = 42
    inline val APPLIEDTYPEtree = 43
    inline val TYPEBOUNDStree = 44
    inline val EXISTENTIALTYPEtree = 45

  inline val MODIFIERS = 50

  inline val firstSymTag = NONEsym
  inline val lastSymTag = VALsym
  inline val lastExtSymTag = EXTMODCLASSref
}


  //The following two are no longer accurate, because ANNOTATEDtpe,
  //SUPERtpe, ... are not in the same range as the other types
  //inline val firstTypeTag = NOtpe
  //inline val lastTypeTag = POLYtpe
