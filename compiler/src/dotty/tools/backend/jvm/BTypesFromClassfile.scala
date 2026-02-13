/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package dotty.tools
package backend.jvm

import scala.annotation.{switch, unused}
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.tools.asm.Opcodes
import scala.tools.asm.tree.{ClassNode, InnerClassNode}
import dotty.tools.backend.jvm.BTypes.{InlineInfo, InternalName, MethodInlineInfo}
import dotty.tools.backend.jvm.BackendReporting.NoClassBTypeInfo
import dotty.tools.backend.jvm.PostProcessorFrontendAccess.Lazy

class BTypesFromClassfile(val byteCodeRepository: BCodeRepository, ts: CoreBTypes) {

  /**
   * Obtain the BType for a type descriptor or internal name. For class descriptors, the ClassBType
   * is constructed by parsing the corresponding classfile.
   *
   * Some JVM operations use either a full descriptor or only an internal name. Example:
   *   ANEWARRAY java/lang/String    // a new array of strings (internal name for the String class)
   *   ANEWARRAY [Ljava/lang/String; // a new array of array of string (full descriptor for the String class)
   *
   * This method supports both descriptors and internal names.
   */
  def bTypeForDescriptorOrInternalNameFromClassfile(descOrIntN: String): BType = (descOrIntN(0): @switch) match {
    case '['                           => ArrayBType(bTypeForDescriptorFromClassfile(descOrIntN.substring(1)))
    case 'L' if descOrIntN.last == ';' => bTypeForDescriptorFromClassfile(descOrIntN)
    case _                             => classBTypeFromParsedClassfile(descOrIntN)
  }

  def bTypeForDescriptorFromClassfile(desc: String): BType = (desc(0): @switch) match {
    case 'V'                     => UNIT
    case 'Z'                     => BOOL
    case 'C'                     => CHAR
    case 'B'                     => BYTE
    case 'S'                     => SHORT
    case 'I'                     => INT
    case 'F'                     => FLOAT
    case 'J'                     => LONG
    case 'D'                     => DOUBLE
    case '['                     => ArrayBType(bTypeForDescriptorFromClassfile(desc.substring(1)))
    case 'L' if desc.last == ';' => classBTypeFromParsedClassfile(desc.substring(1, desc.length - 1))
    case _                       => throw new IllegalArgumentException(s"Not a descriptor: $desc")
  }

  /**
   * Parse the classfile for `internalName` and construct the [[BTypes.ClassBType]]. If the classfile cannot
   * be found in the `byteCodeRepository`, the `info` of the resulting ClassBType is undefined.
   */
  def classBTypeFromParsedClassfile(internalName: InternalName): ClassBType = {
    // JLS §4.1 "There is also a special null type, the type of the expression null [...]
    //           In practice, the programmer can ignore the null type and just pretend that null is merely a special literal that can be of any reference type."
    if internalName == "null" then ts.ObjectRef
    else ts.classBType(internalName, internalName, fromSymbol = false) { (_, n) =>
      byteCodeRepository.classNode(n) match {
        case Left(msg) => Left(NoClassBTypeInfo(msg))
        case Right(c) => computeClassInfoFromClassNode(c)
      }
    }
  }

  /**
   * Construct the [[BTypes.ClassBType]] for a parsed classfile.
   */
  def classBTypeFromClassNode(classNode: ClassNode): ClassBType = {
    ts.classBType(classNode.name, classNode, fromSymbol = false) { (_, cn) =>
      computeClassInfoFromClassNode(cn)
    }
  }

  private def computeClassInfoFromClassNode(classNode: ClassNode): Right[Nothing, ClassInfo] = {
    val superClass = classNode.superName match {
      case null =>
        assert(classNode.name == ts.ObjectRef.internalName, s"class with missing super type: ${classNode.name}")
        None
      case superName =>
        Some(classBTypeFromParsedClassfile(superName))
    }

    val flags = classNode.access

    /*
     * Find all nested classes of classNode. The innerClasses attribute contains all nested classes
     * that are declared inside classNode or used in the bytecode of classNode. So some of them are
     * nested in some other class than classNode, and we need to filter them.
     *
     * For member classes, innerClassNode.outerName is defined, so we compare that to classNode.name.
     *
     * For local and anonymous classes, innerClassNode.outerName is null. Such classes are required
     * to have an EnclosingMethod attribute declaring the outer class. So we keep those local and
     * anonymous classes whose outerClass is classNode.name.
     */
    def nestedInCurrentClass(innerClassNode: InnerClassNode): Boolean = {
      (innerClassNode.outerName != null && innerClassNode.outerName == classNode.name) ||
      (innerClassNode.outerName == null && {
        val classNodeForInnerClass = byteCodeRepository.classNode(innerClassNode.name).get // TODO: don't `get` here, but set the info to Left at the end
        classNodeForInnerClass.outerClass == classNode.name
      })
    }

    def nestedClasses: List[ClassBType] = classNode.innerClasses.asScala.iterator.collect({
      case i if nestedInCurrentClass(i) => classBTypeFromParsedClassfile(i.name)
    }).toList

    // if classNode is a nested class, it has an innerClass attribute for itself. in this
    // case we build the NestedInfo.
    def nestedInfo = classNode.innerClasses.asScala.find(_.name == classNode.name) map {
      innerEntry =>
        val enclosingClass =
          if (innerEntry.outerName != null) {
            // if classNode is a member class, the outerName is non-null
            classBTypeFromParsedClassfile(innerEntry.outerName)
          } else {
            // for anonymous or local classes, the outerName is null, but the enclosing class is
            // stored in the EnclosingMethod attribute (which ASM encodes in classNode.outerClass).
            classBTypeFromParsedClassfile(classNode.outerClass)
          }
        val staticFlag = (innerEntry.access & Opcodes.ACC_STATIC) != 0
        NestedInfo(enclosingClass, Option(innerEntry.outerName), Option(innerEntry.innerName), staticFlag)
    }

    val interfaces: List[ClassBType] = classNode.interfaces.asScala.iterator.map(classBTypeFromParsedClassfile).toList

    Right(ClassInfo(superClass, interfaces, flags, nestedClasses, nestedInfo, InlineInfoSource.Classfile(classNode)))
  }
}
