package scala.annotation
package internal

import language.experimental.captureChecking

/** An annotation attached by the compiler to Java record classes, listing the
 *  names of the record components, in order, and whether the last component is
 *  a repeated (vararg) parameter.
 *
 *  The component names are needed to type and compile record patterns: the
 *  sub-patterns are matched against the record's component accessor methods.
 *  They are attached as an annotation so that they are also available for
 *  record symbols unpickled from TASTy, e.g. in pipelined compilation, where
 *  neither the Java source nor the class file (with its `Record` attribute)
 *  of an upstream record is available.
 *
 *  @param isVararg whether the last record component is a repeated (vararg) parameter
 *  @param names    the names of the record components, in order
 */
class JavaRecordFields(isVararg: Boolean, names: String*) extends Annotation
