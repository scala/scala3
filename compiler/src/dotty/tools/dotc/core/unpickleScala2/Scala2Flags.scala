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
  inline val IMPLICIT      = 1 << 9
  inline val FINAL         = 1 << 5    // May not be overridden. Note that java final implies much more than scala final.
  inline val PRIVATE       = 1 << 2
  inline val PROTECTED     = 1 << 0

  inline val SEALED        = 1 << 10
  inline val OVERRIDE      = 1 << 1
  inline val CASE          = 1 << 11
  inline val ABSTRACT      = 1 << 3        // abstract class, or used in conjunction with abstract override.
                                          // Note difference to DEFERRED!
  inline val DEFERRED      = 1 << 4        // was `abstract' for members | trait is virtual
  inline val INTERFACE     = 1 << 7        // symbol is an interface (i.e. a trait which defines only abstract methods)
  inline val MUTABLE       = 1 << 12       // symbol is a mutable variable.
  inline val PARAM         = 1 << 13       // symbol is a (value or type) parameter to a method
  inline val MACRO         = 1 << 15       // symbol is a macro definition

  inline val COVARIANT     = 1 << 16       // symbol is a covariant type variable
  inline val BYNAMEPARAM   = 1 << 16       // parameter is by name
  inline val CONTRAVARIANT = 1 << 17       // symbol is a contravariant type variable
  inline val ABSOVERRIDE   = 1 << 18       // combination of abstract & override
  inline val LOCAL         = 1 << 19       // symbol is local to current class (i.e. private[this] or protected
                                          // pre: PRIVATE or PROTECTED are also set
  inline val JAVA          = 1 << 20       // symbol was defined by a Java class
  inline val STATIC        = 1 << 23       // static field, method or class
  inline val CASEACCESSOR  = 1 << 24       // symbol is a case parameter (or its accessor, or a GADT skolem)
  inline val TRAIT         = 1 << 25       // symbol is a trait
  inline val DEFAULTPARAM  = 1 << 25       // the parameter has a default value
  inline val PARAMACCESSOR = 1 << 29       // for field definitions generated for primary constructor
                                          //   parameters (no matter if it's a 'val' parameter or not)
                                          // for parameters of a primary constructor ('val' or not)
                                          // for the accessor methods generated for 'val' or 'var' parameters
  inline val LAZY          = 1L << 31      // symbol is a lazy val. can't have MUTABLE unless transformed by typer
  inline val PRESUPER      = 1L << 37      // value is evaluated before super call
  inline val DEFAULTINIT   = 1L << 41      // symbol is initialized to the default value: used by -Xcheckinit
  inline val ARTIFACT      = 1L << 46      // symbol should be ignored when typechecking; will be marked ACC_SYNTHETIC in bytecode
                                          // to see which symbols are marked as ARTIFACT, see scaladocs for FlagValues.ARTIFACT
  inline val DEFAULTMETHOD = 1L << 47      // symbol is a java default method
  inline val ENUM          = 1L << 48      // symbol is an enum

  inline val PrivateLocal   = PRIVATE | LOCAL
  inline val ProtectedLocal = PROTECTED | LOCAL
  inline val AccessFlags    = PRIVATE | PROTECTED | LOCAL

  inline val METHOD        = 1 << 6        // a method
  inline val MODULE        = 1 << 8        // symbol is module or class implementing a module
  inline val PACKAGE       = 1 << 14       // symbol is a java package

  inline val CAPTURED      = 1 << 16       // variable is accessed from nested function.  Set by LambdaLift.
  inline val LABEL         = 1 << 17       // method symbol is a label. Set by TailCall
  inline val INCONSTRUCTOR = 1 << 17       // class symbol is defined in this/superclass constructor.
  inline val SYNTHETIC     = 1 << 21       // symbol is compiler-generated (compare with ARTIFACT)
  inline val STABLE        = 1 << 22       // functions that are assumed to be stable
                                          // (typically, access methods for valdefs)
                                          // or classes that do not contain abstract types.
  inline val BRIDGE        = 1 << 26       // function is a bridge method. Set by Erasure
  inline val ACCESSOR      = 1 << 27       // a value or variable accessor (getter or setter)

  inline val SUPERACCESSOR = 1 << 28       // a super accessor
  inline val MODULEVAR     = 1 << 30       // for variables: is the variable caching a module value

  inline val IS_ERROR      = 1L << 32      // symbol is an error symbol
  inline val OVERLOADED    = 1L << 33      // symbol is overloaded
  inline val LIFTED        = 1L << 34      // class has been lifted out to package level
                                          // local value has been lifted out to class level
                                          // todo: make LIFTED = latePRIVATE?
  inline val MIXEDIN       = 1L << 35      // term member has been mixed in
  inline val EXISTENTIAL   = 1L << 35      // type is an existential parameter or skolem
  inline val EXPANDEDNAME  = 1L << 36      // name has been expanded with class suffix
  inline val TRANS_FLAG    = 1L << 38      // transient flag guaranteed to be reset after each phase.

  inline val LOCKED        = 1L << 39      // temporary flag to catch cyclic dependencies
  inline val SPECIALIZED   = 1L << 40      // symbol is a generated specialized member
  inline val VBRIDGE       = 1L << 42      // symbol is a varargs bridge

  inline val VARARGS       = 1L << 43      // symbol is a Java-style varargs method
  inline val TRIEDCOOKING  = 1L << 44      // `Cooking` has been tried on this symbol
                                          // A Java method's type is `cooked` by transforming raw types to existentials

  inline val SYNCHRONIZED  = 1L << 45      // symbol is a method which should be marked ACC_SYNCHRONIZED

  inline val IMPLICIT_PKL   = (1 << 0)
  inline val FINAL_PKL      = (1 << 1)
  inline val PRIVATE_PKL    = (1 << 2)
  inline val PROTECTED_PKL  = (1 << 3)
  inline val SEALED_PKL     = (1 << 4)
  inline val OVERRIDE_PKL   = (1 << 5)
  inline val CASE_PKL       = (1 << 6)
  inline val ABSTRACT_PKL   = (1 << 7)
  inline val DEFERRED_PKL   = (1 << 8)
  inline val METHOD_PKL     = (1 << 9)
  inline val MODULE_PKL     = (1 << 10)
  inline val INTERFACE_PKL  = (1 << 11)
}
