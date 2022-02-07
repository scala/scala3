package dotty.tools.dotc
package core
package classfile

import scala.annotation.switch

object ClassfileConstants {

  inline val JAVA_MAGIC = 0xCAFEBABE
  inline val JAVA_MAJOR_VERSION = 45
  inline val JAVA_MINOR_VERSION = 3

  inline val JAVA8_MAJOR_VERSION = 52

  /** (see http://java.sun.com/docs/books/jvms/second_edition/jvms-clarify.html)
   *
   *  If the `ACC_INTERFACE` flag is set, the `ACC_ABSTRACT` flag must also
   *  be set (ch. 2.13.1).
   *
   *  A class file cannot have both its `ACC_FINAL` and `ACC_ABSTRACT` flags
   *  set (ch. 2.8.2).
   *
   *  A field may have at most one of its `ACC_PRIVATE`, `ACC_PROTECTED`,
   *  `ACC_PUBLIC` flags set (ch. 2.7.4).
   *
   *  A field may not have both its `ACC_FINAL` and `ACC_VOLATILE` flags set
   *  (ch. 2.9.1).
   *
   *  If a method has its `ACC_ABSTRACT` flag set it must not have any of its
   *  `ACC_FINAL`, `ACC_NATIVE`, `ACC_PRIVATE`, `ACC_STATIC`, `ACC_STRICT`,
   *  or `ACC_SYNCHRONIZED` flags set (ch. 2.13.3.2).
   *
   *  All interface methods must have their `ACC_ABSTRACT` and
   *  `ACC_PUBLIC` flags set.
   *
   *  Note for future reference: see this thread on ACC_SUPER and
   *  how its enforcement differs on the android vm.
   *    https://groups.google.com/forum/?hl=en#!topic/jvm-languages/jVhzvq8-ZIk
   *
   */                                        // Class   Field   Method
  inline val JAVA_ACC_PUBLIC       = 0x0001   //   X       X        X
  inline val JAVA_ACC_PRIVATE      = 0x0002   //           X        X
  inline val JAVA_ACC_PROTECTED    = 0x0004   //           X        X
  inline val JAVA_ACC_STATIC       = 0x0008   //           X        X
  inline val JAVA_ACC_FINAL        = 0x0010   //   X       X        X
  inline val JAVA_ACC_SUPER        = 0x0020   //   X
  inline val JAVA_ACC_SYNCHRONIZED = 0x0020   //                    X
  inline val JAVA_ACC_VOLATILE     = 0x0040   //           X
  inline val JAVA_ACC_BRIDGE       = 0x0040   //                    X
  inline val JAVA_ACC_TRANSIENT    = 0x0080   //           X
  inline val JAVA_ACC_VARARGS      = 0x0080   //                    X
  inline val JAVA_ACC_NATIVE       = 0x0100   //                    X
  inline val JAVA_ACC_INTERFACE    = 0x0200   //   X
  inline val JAVA_ACC_ABSTRACT     = 0x0400   //   X                X
  inline val JAVA_ACC_STRICT       = 0x0800   //                    X
  inline val JAVA_ACC_SYNTHETIC    = 0x1000   //   X       X        X
  inline val JAVA_ACC_ANNOTATION   = 0x2000   //   X
  inline val JAVA_ACC_ENUM         = 0x4000   //   X       X

  // tags describing the type of a literal in the constant pool
  inline val CONSTANT_UTF8          =  1
  inline val CONSTANT_UNICODE       =  2
  inline val CONSTANT_INTEGER       =  3
  inline val CONSTANT_FLOAT         =  4
  inline val CONSTANT_LONG          =  5
  inline val CONSTANT_DOUBLE        =  6
  inline val CONSTANT_CLASS         =  7
  inline val CONSTANT_STRING        =  8
  inline val CONSTANT_FIELDREF      =  9
  inline val CONSTANT_METHODREF     = 10
  inline val CONSTANT_INTFMETHODREF = 11
  inline val CONSTANT_NAMEANDTYPE   = 12

  inline val CONSTANT_METHODHANDLE  = 15
  inline val CONSTANT_METHODTYPE    = 16
  inline val CONSTANT_INVOKEDYNAMIC = 18

  // tags describing the type of a literal in attribute values
  inline val BYTE_TAG   = 'B'
  inline val CHAR_TAG   = 'C'
  inline val DOUBLE_TAG = 'D'
  inline val FLOAT_TAG  = 'F'
  inline val INT_TAG    = 'I'
  inline val LONG_TAG   = 'J'
  inline val SHORT_TAG  = 'S'
  inline val BOOL_TAG   = 'Z'
  inline val STRING_TAG = 's'
  inline val ENUM_TAG   = 'e'
  inline val CLASS_TAG  = 'c'
  inline val ARRAY_TAG  = '['
  inline val VOID_TAG   = 'V'
  inline val TVAR_TAG   = 'T'
  inline val OBJECT_TAG = 'L'
  inline val ANNOTATION_TAG = '@'
  inline val SCALA_NOTHING = "scala.runtime.Nothing$"
  inline val SCALA_NULL = "scala.runtime.Null$"


  // tags describing the type of newarray
  inline val T_BOOLEAN = 4
  inline val T_CHAR    = 5
  inline val T_FLOAT   = 6
  inline val T_DOUBLE  = 7
  inline val T_BYTE    = 8
  inline val T_SHORT   = 9
  inline val T_INT     = 10
  inline val T_LONG    = 11

  // JVM mnemonics
  inline val nop         = 0x00
  inline val aconst_null = 0x01
  inline val iconst_m1   = 0x02

  inline val iconst_0    = 0x03
  inline val iconst_1    = 0x04
  inline val iconst_2    = 0x05
  inline val iconst_3    = 0x06
  inline val iconst_4    = 0x07
  inline val iconst_5    = 0x08

  inline val lconst_0    = 0x09
  inline val lconst_1    = 0x0a
  inline val fconst_0    = 0x0b
  inline val fconst_1    = 0x0c
  inline val fconst_2    = 0x0d
  inline val dconst_0    = 0x0e
  inline val dconst_1    = 0x0f

  inline val bipush      = 0x10
  inline val sipush      = 0x11
  inline val ldc         = 0x12
  inline val ldc_w       = 0x13
  inline val ldc2_w      = 0x14

  inline val iload       = 0x15
  inline val lload       = 0x16
  inline val fload       = 0x17
  inline val dload       = 0x18
  inline val aload       = 0x19

  inline val iload_0     = 0x1a
  inline val iload_1     = 0x1b
  inline val iload_2     = 0x1c
  inline val iload_3     = 0x1d
  inline val lload_0     = 0x1e
  inline val lload_1     = 0x1f
  inline val lload_2     = 0x20
  inline val lload_3     = 0x21
  inline val fload_0     = 0x22
  inline val fload_1     = 0x23
  inline val fload_2     = 0x24
  inline val fload_3     = 0x25
  inline val dload_0     = 0x26
  inline val dload_1     = 0x27
  inline val dload_2     = 0x28
  inline val dload_3     = 0x29
  inline val aload_0     = 0x2a
  inline val aload_1     = 0x2b
  inline val aload_2     = 0x2c
  inline val aload_3     = 0x2d
  inline val iaload      = 0x2e
  inline val laload      = 0x2f
  inline val faload      = 0x30
  inline val daload      = 0x31
  inline val aaload      = 0x32
  inline val baload      = 0x33
  inline val caload      = 0x34
  inline val saload      = 0x35

  inline val istore      = 0x36
  inline val lstore      = 0x37
  inline val fstore      = 0x38
  inline val dstore      = 0x39
  inline val astore      = 0x3a
  inline val istore_0    = 0x3b
  inline val istore_1    = 0x3c
  inline val istore_2    = 0x3d
  inline val istore_3    = 0x3e
  inline val lstore_0    = 0x3f
  inline val lstore_1    = 0x40
  inline val lstore_2    = 0x41
  inline val lstore_3    = 0x42
  inline val fstore_0    = 0x43
  inline val fstore_1    = 0x44
  inline val fstore_2    = 0x45
  inline val fstore_3    = 0x46
  inline val dstore_0    = 0x47
  inline val dstore_1    = 0x48
  inline val dstore_2    = 0x49
  inline val dstore_3    = 0x4a
  inline val astore_0    = 0x4b
  inline val astore_1    = 0x4c
  inline val astore_2    = 0x4d
  inline val astore_3    = 0x4e
  inline val iastore     = 0x4f
  inline val lastore     = 0x50
  inline val fastore     = 0x51
  inline val dastore     = 0x52
  inline val aastore     = 0x53
  inline val bastore     = 0x54
  inline val castore     = 0x55
  inline val sastore     = 0x56

  inline val pop         = 0x57
  inline val pop2        = 0x58
  inline val dup         = 0x59
  inline val dup_x1      = 0x5a
  inline val dup_x2      = 0x5b
  inline val dup2        = 0x5c
  inline val dup2_x1     = 0x5d
  inline val dup2_x2     = 0x5e
  inline val swap        = 0x5f

  inline val iadd        = 0x60
  inline val ladd        = 0x61
  inline val fadd        = 0x62
  inline val dadd        = 0x63
  inline val isub        = 0x64
  inline val lsub        = 0x65
  inline val fsub        = 0x66
  inline val dsub        = 0x67
  inline val imul        = 0x68
  inline val lmul        = 0x69
  inline val fmul        = 0x6a
  inline val dmul        = 0x6b
  inline val idiv        = 0x6c
  inline val ldiv        = 0x6d
  inline val fdiv        = 0x6e
  inline val ddiv        = 0x6f
  inline val irem        = 0x70
  inline val lrem        = 0x71
  inline val frem        = 0x72
  inline val drem        = 0x73

  inline val ineg        = 0x74
  inline val lneg        = 0x75
  inline val fneg        = 0x76
  inline val dneg        = 0x77

  inline val ishl        = 0x78
  inline val lshl        = 0x79
  inline val ishr        = 0x7a
  inline val lshr        = 0x7b
  inline val iushr       = 0x7c
  inline val lushr       = 0x7d
  inline val iand        = 0x7e
  inline val land        = 0x7f
  inline val ior         = 0x80
  inline val lor         = 0x81
  inline val ixor        = 0x82
  inline val lxor        = 0x83
  inline val iinc        = 0x84

  inline val i2l         = 0x85
  inline val i2f         = 0x86
  inline val i2d         = 0x87
  inline val l2i         = 0x88
  inline val l2f         = 0x89
  inline val l2d         = 0x8a
  inline val f2i         = 0x8b
  inline val f2l         = 0x8c
  inline val f2d         = 0x8d
  inline val d2i         = 0x8e
  inline val d2l         = 0x8f
  inline val d2f         = 0x90
  inline val i2b         = 0x91
  inline val i2c         = 0x92
  inline val i2s         = 0x93

  inline val lcmp        = 0x94
  inline val fcmpl       = 0x95
  inline val fcmpg       = 0x96
  inline val dcmpl       = 0x97
  inline val dcmpg       = 0x98

  inline val ifeq        = 0x99
  inline val ifne        = 0x9a
  inline val iflt        = 0x9b
  inline val ifge        = 0x9c
  inline val ifgt        = 0x9d
  inline val ifle        = 0x9e
  inline val if_icmpeq   = 0x9f
  inline val if_icmpne   = 0xa0
  inline val if_icmplt   = 0xa1
  inline val if_icmpge   = 0xa2
  inline val if_icmpgt   = 0xa3
  inline val if_icmple   = 0xa4
  inline val if_acmpeq   = 0xa5
  inline val if_acmpne   = 0xa6
  inline val goto        = 0xa7
  inline val jsr         = 0xa8
  inline val ret         = 0xa9
  inline val tableswitch = 0xaa
  inline val lookupswitch = 0xab
  inline val ireturn     = 0xac
  inline val lreturn     = 0xad
  inline val freturn     = 0xae
  inline val dreturn     = 0xaf
  inline val areturn     = 0xb0
  inline val return_     = 0xb1

  inline val getstatic   = 0xb2
  inline val putstatic   = 0xb3
  inline val getfield    = 0xb4
  inline val putfield    = 0xb5

  inline val invokevirtual   = 0xb6
  inline val invokespecial   = 0xb7
  inline val invokestatic    = 0xb8
  inline val invokeinterface = 0xb9
  inline val xxxunusedxxxx   = 0xba

  inline val new_          = 0xbb
  inline val newarray      = 0xbc
  inline val anewarray     = 0xbd
  inline val arraylength   = 0xbe
  inline val athrow        = 0xbf
  inline val checkcast     = 0xc0
  inline val instanceof    = 0xc1
  inline val monitorenter  = 0xc2
  inline val monitorexit   = 0xc3
  inline val wide          = 0xc4
  inline val multianewarray = 0xc5
  inline val ifnull        = 0xc6
  inline val ifnonnull     = 0xc7
  inline val goto_w        = 0xc8
  inline val jsr_w         = 0xc9

  // reserved opcodes
  inline val breakpoint    = 0xca
  inline val impdep1       = 0xfe
  inline val impdep2       = 0xff

  import Flags._
  abstract class FlagTranslation {

    protected def baseFlags(jflags: Int): FlagSet = EmptyFlags
    protected def isClass: Boolean = false

    private def translateFlag(jflag: Int): FlagSet = (jflag: @switch) match {
      case JAVA_ACC_PRIVATE    => Private
      case JAVA_ACC_PROTECTED  => Protected
      case JAVA_ACC_FINAL      => Final
      case JAVA_ACC_SYNTHETIC  => SyntheticArtifact
      case JAVA_ACC_STATIC     => JavaStatic
      case JAVA_ACC_ENUM       => Enum
      case JAVA_ACC_ABSTRACT   => if (isClass) Abstract else Deferred
      case JAVA_ACC_INTERFACE  => PureInterfaceCreationFlags | JavaDefined
      case _                   => EmptyFlags
    }

    private def addFlag(base: FlagSet, jflag: Int): FlagSet =
      if (jflag == 0) base else base | translateFlag(jflag)

    private def translateFlags(jflags: Int, baseFlags: FlagSet): FlagSet = {
      val nflags =
        if ((jflags & JAVA_ACC_ANNOTATION) == 0) jflags
        else jflags & ~(JAVA_ACC_ABSTRACT | JAVA_ACC_INTERFACE) // annotations are neither abstract nor interfaces
      var res: FlagSet = baseFlags | JavaDefined
      res = addFlag(res, nflags & JAVA_ACC_PRIVATE)
      res = addFlag(res, nflags & JAVA_ACC_PROTECTED)
      res = addFlag(res, nflags & JAVA_ACC_FINAL)
      res = addFlag(res, nflags & JAVA_ACC_SYNTHETIC)
      res = addFlag(res, nflags & JAVA_ACC_STATIC)
      res = addFlag(res, nflags & JAVA_ACC_ENUM)
      res = addFlag(res, nflags & JAVA_ACC_ABSTRACT)
      res = addFlag(res, nflags & JAVA_ACC_INTERFACE)
      res
    }

    def flags(jflags: Int): FlagSet = translateFlags(jflags, baseFlags(jflags))
  }
  val classTranslation: FlagTranslation = new FlagTranslation {
    override def isClass = true
  }
  val fieldTranslation: FlagTranslation = new FlagTranslation {
    override def baseFlags(jflags: Int) = if ((jflags & JAVA_ACC_FINAL) == 0) Mutable else EmptyFlags
  }
  val methodTranslation: FlagTranslation = new FlagTranslation {
    override def baseFlags(jflags: Int) = if ((jflags & JAVA_ACC_BRIDGE) != 0) Bridge else EmptyFlags
  }
}
