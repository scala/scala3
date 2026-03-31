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

package dotty.tools.backend.jvm.opt

import dotty.tools.backend.jvm.BTypes.InternalName
import dotty.tools.backend.jvm.PostProcessorFrontendAccess.Lazy
import dotty.tools.backend.jvm.opt.{BCodeRepository, ClassNotFound, NoClassBTypeInfo, OptimizerWarning}
import dotty.tools.backend.jvm.*
import dotty.tools.dotc.core.StdNames.nme

import scala.annotation.{switch, unused}
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.tools.asm.Opcodes
import scala.tools.asm.tree.{ClassNode, InnerClassNode, ModuleNode}

class BTypesFromClassfile(val byteCodeRepository: BCodeRepository, ts: CoreBTypes) extends InlineInfoLoader {

  /**
   * Obtain the BType for a type descriptor or internal name. For class descriptors, the ClassBType
   * is constructed by parsing the corresponding classfile.
   *
   * Some JVM operations use either a full descriptor or only an internal name. Example:
   *   ANEWARRAY java/lang/String    // a new array of strings (internal name for the String class)
   *   ANEWARRAY [Ljava/lang/String; // a new array of arrays of string (full descriptor for the String class)
   *
   * This method supports both descriptors and internal names.
   */
  def bTypeForDescriptorOrInternalNameFromClassfile(descOrIntN: String): Either[OptimizerWarning, BType] = (descOrIntN(0): @switch) match {
    case '['                           => bTypeForDescriptorFromClassfile(descOrIntN.substring(1)).map(ArrayBType.apply)
    case 'L' if descOrIntN.last == ';' => bTypeForDescriptorFromClassfile(descOrIntN)
    case _                             => classBTypeFromParsedClassfile(descOrIntN)
  }

  def bTypeForDescriptorFromClassfile(desc: String): Either[OptimizerWarning, BType] = (desc(0): @switch) match {
    case 'V'                     => Right(UNIT)
    case 'Z'                     => Right(BOOL)
    case 'C'                     => Right(CHAR)
    case 'B'                     => Right(BYTE)
    case 'S'                     => Right(SHORT)
    case 'I'                     => Right(INT)
    case 'F'                     => Right(FLOAT)
    case 'J'                     => Right(LONG)
    case 'D'                     => Right(DOUBLE)
    case '['                     => bTypeForDescriptorFromClassfile(desc.substring(1)).map(ArrayBType.apply)
    case 'L' if desc.last == ';' => classBTypeFromParsedClassfile(desc.substring(1, desc.length - 1))
    case _                       => throw new IllegalArgumentException(s"Not a descriptor: $desc")
  }

  /**
   * Parse the classfile for `internalName` and construct the [[BTypes.ClassBType]]. If the classfile cannot
   * be found in the `byteCodeRepository`, the `info` of the resulting ClassBType is undefined.
   */
  def classBTypeFromParsedClassfile(internalName: InternalName): Either[OptimizerWarning, ClassBType] = {
    // JLS §4.1 "There is also a special null type, the type of the expression null [...]
    //           In practice, the programmer can ignore the null type and just pretend that null is merely a special literal that can be of any reference type."
    if internalName == "null" then Right(ts.ObjectRef)
    else ts.classBType(internalName) { _ =>
      byteCodeRepository.classNode(internalName) match {
        case Left(msg) => Left(NoClassBTypeInfo(msg))
        case Right(c, m) => computeClassInfoFromClassNode(c, m)
      }
    }
  }

  /**
   * Construct the [[BTypes.ClassBType]] for a parsed classfile.
   */
  def classBTypeFromClassNode(classNode: ClassNode, moduleNode: Option[ModuleNode]): Either[OptimizerWarning, ClassBType] = {
    ts.classBType(classNode.name) { _ =>
      computeClassInfoFromClassNode(classNode, moduleNode)
    }
  }

  private def computeClassInfoFromClassNode(classNode: ClassNode, moduleNode: Option[ModuleNode]): Either[OptimizerWarning, ClassInfo] = {
    val superClass = classNode.superName match {
      case null =>
        assert(classNode.name == ts.ObjectRef.internalName, s"class with missing super type: ${classNode.name}")
        Right(None)
      case superName =>
        classBTypeFromParsedClassfile(superName).map(Some.apply)
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
    def nestedInCurrentClass(innerClassNode: InnerClassNode): Either[OptimizerWarning, Boolean] = {
      if innerClassNode.outerName != null && innerClassNode.outerName == classNode.name then
        Right(true)
      else if innerClassNode.outerName == null then
        byteCodeRepository.classNode(innerClassNode.name).map { case (classNodeForInnerClass, _) =>
          classNodeForInnerClass.outerClass == classNode.name
        }
      else
        Right(false)
    }
    
    def collect(it: Iterator[Either[OptimizerWarning, ClassBType]]): Either[OptimizerWarning, List[ClassBType]] =
      it.foldLeft(Right(Nil).asInstanceOf[Either[OptimizerWarning, List[ClassBType]]]){
        case (Right(xs), Right(x)) => Right(x :: xs)
        case (Left(l), _) => Left(l)
        case (_, Left(l)) => Left(l)
      }.map(_.reverse)

    def nestedClasses: Either[OptimizerWarning, List[ClassBType]] = 
      collect(classNode.innerClasses.asScala.iterator.collect(Function.unlift(i =>
        nestedInCurrentClass(i) match
          case Left(l) => Some(Left(l))
          case Right(false) => None
          case Right(true) => Some(classBTypeFromParsedClassfile(i.name))
      )))

    // if classNode is a nested class, it has an innerClass attribute for itself. in this
    // case we build the NestedInfo.
    def nestedInfo = classNode.innerClasses.asScala.find(_.name == classNode.name) match {
      case Some(innerEntry) =>
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
        enclosingClass.map(ec => Some(NestedInfo(ec, Option(innerEntry.outerName), Option(innerEntry.innerName), staticFlag)))
      case None =>
        Right(None)
    }

    val interfaces = collect(classNode.interfaces.asScala.iterator.map(classBTypeFromParsedClassfile))

    (superClass, interfaces, nestedClasses, nestedInfo) match
      case (Right(sc), Right(is), Right(ncs), Right(ni)) => Right(ClassInfo(sc, is, flags, ncs, ni, inlineInfoFromClassfile(classNode, moduleNode)))
      case (Left(l), _, _, _) => Left(l)
      case (_, Left(l), _, _) => Left(l)
      case (_, _, Left(l), _) => Left(l)
      case (_, _, _, Left(l)) => Left(l)
  }

  def loadInlineInfoFor(name: InternalName): InlineInfo =
    byteCodeRepository.classNode(name) match {
      case Right(classNode, moduleNode) =>
        inlineInfoFromClassfile(classNode, moduleNode)
      case Left(missingClass) =>
        InlineInfo.empty.copy(warning = Some(ClassNotFoundWhenBuildingInlineInfoFromSymbol(missingClass)))
    }
  
  /**
   * Build the InlineInfo for a class. For Scala classes, the information is stored in the
   * ScalaInlineInfo attribute. If the attribute is missing, the InlineInfo is built using the
   * metadata available in the classfile (ACC_FINAL flags, etc.).
   */
  private def inlineInfoFromClassfile(classNode: ClassNode, moduleNode: Option[ModuleNode]): InlineInfo = {
    def fromClassfileAttribute: Option[InlineInfo] = {
      if (classNode.attrs == null) None
      else classNode.attrs.asScala.collectFirst { case a: InlineInfoAttribute => a.inlineInfo }
    }

    def fromClassfileWithoutAttribute = {
      val warning = {
        val isScala = classNode.attrs != null && classNode.attrs.asScala.exists(a => a.`type` == nme.ScalaATTR.toString || a.`type` == nme.ScalaSignatureATTR.toString)
        if (isScala) Some(NoInlineInfoAttribute(classNode.name))
        else None
      }
      // when building MethodInlineInfos for the members of a ClassSymbol, we exclude those methods
      // in scalaPrimitives. This is necessary because some of them have non-erased types, which would
      // require special handling. Excluding is OK because they are never inlined.
      // Here we are parsing from a classfile and we don't need to do anything special. Many of these
      // primitives don't even exist, for example Any.isInstanceOf.
      val methodInfos = new mutable.TreeMap[(String, String), MethodInlineInfo]()
      val isFinalClass = BCodeUtils.isFinalClass(classNode)
      classNode.methods.forEach(methodNode => {
        val info = MethodInlineInfo(effectivelyFinal = isFinalClass || BCodeUtils.isFinalMethod(methodNode))
        methodInfos((methodNode.name, methodNode.desc)) = info
      })

      // We only want an approximation of SAMs for inlining heuristics, no need to check FunctionalInterface annotations or such
      val abstractMethods = classNode.methods.asScala.filter(m => (m.access & Opcodes.ACC_ABSTRACT) != 0)
      val sam = if abstractMethods.size == 1 then Some(abstractMethods.head.name + abstractMethods.head.desc) else None

      // No module => always accessible
      val isAccessible = moduleNode.forall(m =>
        val nodePackageName = classNode.name.substring(0, classNode.name.lastIndexOf('/'))
        m.exports.asScala.exists(e => (e.modules == null || e.modules.size == 0) && e.packaze == nodePackageName)
      )

      InlineInfo(isFinalClass, sam, methodInfos, warning, isAccessible)
    }

    fromClassfileAttribute.getOrElse(fromClassfileWithoutAttribute)
  }
}
