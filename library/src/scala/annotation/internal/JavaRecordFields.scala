package scala.annotation.internal

import scala.annotation.StaticAnnotation

/** An annotation attached by JavaParsers/ClassfileParser to Java record class
 *  with a list of that record's fields. Used in pattern matching on records.
 */
final class JavaRecordFields(args: String*) extends StaticAnnotation
