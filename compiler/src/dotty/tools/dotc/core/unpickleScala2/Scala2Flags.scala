/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package dotty.tools
package dotc
package core
package unpickleScala2


/** Scala2 flags, adapted from https://github.com/scala/scala/blob/2.11.x/src/reflect/scala/reflect/internal/Flags.scala
 */
object Scala2Flags {
  final val IMPLICIT      = 1 << 9
  final val FINAL         = 1 << 5    // May not be overridden. Note that java final implies much more than scala final.
  final val PRIVATE       = 1 << 2
  final val PROTECTED     = 1 << 0

  final val SEALED        = 1 << 10
  final val OVERRIDE      = 1 << 1
  final val CASE          = 1 << 11
  final val ABSTRACT      = 1 << 3        // abstract class, or used in conjunction with abstract override.
                                          // Note difference to DEFERRED!
  final val DEFERRED      = 1 << 4        // was `abstract' for members | trait is virtual
  final val INTERFACE     = 1 << 7        // symbol is an interface (i.e. a trait which defines only abstract methods)
  final val MUTABLE       = 1 << 12       // symbol is a mutable variable.
  final val PARAM         = 1 << 13       // symbol is a (value or type) parameter to a method
  final val MACRO         = 1 << 15       // symbol is a macro definition

  final val COVARIANT     = 1 << 16       // symbol is a covariant type variable
  final val BYNAMEPARAM   = 1 << 16       // parameter is by name
  final val CONTRAVARIANT = 1 << 17       // symbol is a contravariant type variable
  final val ABSOVERRIDE   = 1 << 18       // combination of abstract & override
  final val LOCAL         = 1 << 19       // symbol is local to current class (i.e. private[this] or protected[this]
                                          // pre: PRIVATE or PROTECTED are also set
  final val JAVA          = 1 << 20       // symbol was defined by a Java class
  final val STATIC        = 1 << 23       // static field, method or class
  final val CASEACCESSOR  = 1 << 24       // symbol is a case parameter (or its accessor, or a GADT skolem)
  final val TRAIT         = 1 << 25       // symbol is a trait
  final val DEFAULTPARAM  = 1 << 25       // the parameter has a default value
  final val PARAMACCESSOR = 1 << 29       // for field definitions generated for primary constructor
                                          //   parameters (no matter if it's a 'val' parameter or not)
                                          // for parameters of a primary constructor ('val' or not)
                                          // for the accessor methods generated for 'val' or 'var' parameters
  final val LAZY          = 1L << 31      // symbol is a lazy val. can't have MUTABLE unless transformed by typer
  final val PRESUPER      = 1L << 37      // value is evaluated before super call
  final val DEFAULTINIT   = 1L << 41      // symbol is initialized to the default value: used by -Xcheckinit
  final val ARTIFACT      = 1L << 46      // symbol should be ignored when typechecking; will be marked ACC_SYNTHETIC in bytecode
                                          // to see which symbols are marked as ARTIFACT, see scaladocs for FlagValues.ARTIFACT
  final val DEFAULTMETHOD = 1L << 47      // symbol is a java default method
  final val ENUM          = 1L << 48      // symbol is an enum

  final val PrivateLocal   = PRIVATE | LOCAL
  final val ProtectedLocal = PROTECTED | LOCAL
  final val AccessFlags    = PRIVATE | PROTECTED | LOCAL

  final val METHOD        = 1 << 6        // a method
  final val MODULE        = 1 << 8        // symbol is module or class implementing a module
  final val PACKAGE       = 1 << 14       // symbol is a java package

  final val CAPTURED      = 1 << 16       // variable is accessed from nested function.  Set by LambdaLift.
  final val LABEL         = 1 << 17       // method symbol is a label. Set by TailCall
  final val INCONSTRUCTOR = 1 << 17       // class symbol is defined in this/superclass constructor.
  final val SYNTHETIC     = 1 << 21       // symbol is compiler-generated (compare with ARTIFACT)
  final val STABLE        = 1 << 22       // functions that are assumed to be stable
                                          // (typically, access methods for valdefs)
                                          // or classes that do not contain abstract types.
  final val BRIDGE        = 1 << 26       // function is a bridge method. Set by Erasure
  final val ACCESSOR      = 1 << 27       // a value or variable accessor (getter or setter)

  final val SUPERACCESSOR = 1 << 28       // a super accessor
  final val MODULEVAR     = 1 << 30       // for variables: is the variable caching a module value

  final val IS_ERROR      = 1L << 32      // symbol is an error symbol
  final val OVERLOADED    = 1L << 33      // symbol is overloaded
  final val LIFTED        = 1L << 34      // class has been lifted out to package level
                                          // local value has been lifted out to class level
                                          // todo: make LIFTED = latePRIVATE?
  final val MIXEDIN       = 1L << 35      // term member has been mixed in
  final val EXISTENTIAL   = 1L << 35      // type is an existential parameter or skolem
  final val EXPANDEDNAME  = 1L << 36      // name has been expanded with class suffix
  final val TRANS_FLAG    = 1L << 38      // transient flag guaranteed to be reset after each phase.

  final val LOCKED        = 1L << 39      // temporary flag to catch cyclic dependencies
  final val SPECIALIZED   = 1L << 40      // symbol is a generated specialized member
  final val VBRIDGE       = 1L << 42      // symbol is a varargs bridge

  final val VARARGS       = 1L << 43      // symbol is a Java-style varargs method
  final val TRIEDCOOKING  = 1L << 44      // `Cooking` has been tried on this symbol
                                          // A Java method's type is `cooked` by transforming raw types to existentials

  final val SYNCHRONIZED  = 1L << 45      // symbol is a method which should be marked ACC_SYNCHRONIZED

  final val IMPLICIT_PKL   = (1 << 0)
  final val FINAL_PKL      = (1 << 1)
  final val PRIVATE_PKL    = (1 << 2)
  final val PROTECTED_PKL  = (1 << 3)
  final val SEALED_PKL     = (1 << 4)
  final val OVERRIDE_PKL   = (1 << 5)
  final val CASE_PKL       = (1 << 6)
  final val ABSTRACT_PKL   = (1 << 7)
  final val DEFERRED_PKL   = (1 << 8)
  final val METHOD_PKL     = (1 << 9)
  final val MODULE_PKL     = (1 << 10)
  final val INTERFACE_PKL  = (1 << 11)
}
