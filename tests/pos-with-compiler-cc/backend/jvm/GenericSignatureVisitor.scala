package dotty.tools.backend.jvm

import scala.language.unsafeNulls

import scala.tools.asm.{ClassReader, Type, Handle }
import scala.tools.asm.tree._

import scala.collection.mutable
import scala.util.control.{NoStackTrace, NonFatal}
import scala.annotation._
import scala.jdk.CollectionConverters._

// Backported from scala/scala, commit sha: 724be0e9425b9ad07c244d25efdad695d75abbcf
// https://github.com/scala/scala/blob/724be0e9425b9ad07c244d25efdad695d75abbcf/src/compiler/scala/tools/nsc/backend/jvm/analysis/BackendUtils.scala#L928
abstract class GenericSignatureVisitor(nestedOnly: Boolean) {
  // For performance (`Char => Boolean` is not specialized)
  private trait CharBooleanFunction { def apply(c: Char): Boolean }

  final def visitInternalName(internalName: String): Unit = visitInternalName(internalName, 0, if (internalName eq null) 0 else internalName.length)
  def visitInternalName(internalName: String, offset: Int, length: Int): Unit

  def raiseError(msg: String, sig: String, e: Option[Throwable] = None): Unit

  def visitClassSignature(sig: String): Unit = if (sig != null) {
    val p = new Parser(sig, nestedOnly)
    p.safely { p.classSignature() }
  }

  def visitMethodSignature(sig: String): Unit = if (sig != null) {
    val p = new Parser(sig, nestedOnly)
    p.safely { p.methodSignature() }
  }

  def visitFieldSignature(sig: String): Unit = if (sig != null) {
    val p = new Parser(sig, nestedOnly)
    p.safely { p.fieldSignature() }
  }

  private final class Parser(sig: String, nestedOnly: Boolean) {

    private var index = 0
    private val end = sig.length

    private val Aborted: Throwable = new NoStackTrace { }
    private def abort(): Nothing = throw Aborted

    @inline def safely(f: => Unit): Unit = try f catch {
      case Aborted =>
      case NonFatal(e) => raiseError(s"Exception thrown during signature parsing", sig, Some(e))
    }

    private def current = {
      if (index >= end) {
        raiseError(s"Out of bounds, $index >= $end", sig)
        abort() // Don't continue, even if `notifyInvalidSignature` returns
      }
      sig.charAt(index)
    }

    private def accept(c: Char): Unit = {
      if (current != c) {
        raiseError(s"Expected $c at $index, found $current", sig)
        abort()
      }
      index += 1
    }

    private def skip(): Unit = { index += 1 }
    private def getCurrentAndSkip(): Char = { val c = current; skip(); c }

    private def skipUntil(isDelimiter: CharBooleanFunction): Unit = {
      while (!isDelimiter(current)) { index += 1 }
    }
    private def skipUntilDelimiter(delimiter: Char): Unit = {
      sig.indexOf(delimiter, index) match {
        case -1 =>
          raiseError(s"Out of bounds", sig)
          abort() // Don't continue, even if `notifyInvalidSignature` returns
        case i =>
          index = i
      }
    }

    private def appendUntil(builder: java.lang.StringBuilder, isDelimiter: CharBooleanFunction): Unit = {
      val start = index
      skipUntil(isDelimiter)
      builder.append(sig, start, index)
    }

    def isBaseType(c: Char): Boolean = c match {
      case 'B' | 'C' | 'D' | 'F' | 'I' | 'J' | 'S' | 'Z' => true
      case _ => false
    }

    private val isClassNameEnd: CharBooleanFunction = (c: Char) => c == '<' || c == '.' || c == ';'

    private def typeArguments(): Unit = if (current == '<') {
      skip()
      while (current != '>') current match {
        case '*' | '+' | '-' =>
          skip()
        case _ =>
          referenceTypeSignature()
      }
      accept('>')
    }

    @tailrec private def referenceTypeSignature(): Unit = getCurrentAndSkip() match {
      case 'L' =>
        var names: java.lang.StringBuilder = null

        val start = index
        var seenDollar = false
        while (!isClassNameEnd(current)) {
          seenDollar ||= current == '$'
          index += 1
        }
        if ((current == '.' || seenDollar) || !nestedOnly) {
          // OPT: avoid allocations when only a top-level class is encountered
          names = new java.lang.StringBuilder(32)
          names.append(sig, start, index)
          visitInternalName(names.toString)
        }
        typeArguments()

        while (current == '.') {
          skip()
          names.append('$')
          appendUntil(names, isClassNameEnd)
          visitInternalName(names.toString)
          typeArguments()
        }
        accept(';')

      case 'T' =>
        skipUntilDelimiter(';')
        skip()

      case '[' =>
        if (isBaseType(current)) skip()
        else referenceTypeSignature()
    }

    private def typeParameters(): Unit = if (current == '<') {
      skip()
      while (current != '>') {
        skipUntilDelimiter(':'); skip()
        val c = current
        // The ClassBound can be missing, but only if there's an InterfaceBound after.
        // This is an assumption that's not in the spec, see https://stackoverflow.com/q/44284928
        if (c != ':' && c != '>') { referenceTypeSignature() }
        while (current == ':') { skip(); referenceTypeSignature() }
      }
      accept('>')
    }

    def classSignature(): Unit = {
      typeParameters()
      while (index < end) referenceTypeSignature()
    }

    def methodSignature(): Unit = {
      typeParameters()

      accept('(')
      while (current != ')') {
        if (isBaseType(current)) skip()
        else referenceTypeSignature()
      }
      accept(')')

      if (current == 'V' || isBaseType(current)) skip()
      else referenceTypeSignature()

      while (index < end) {
        accept('^')
        referenceTypeSignature()
      }
    }

    def fieldSignature(): Unit = if (sig != null) safely {
      referenceTypeSignature()
    }
  }
}

// Backported from scala/scala, commit sha: 724be0e9425b9ad07c244d25efdad695d75abbcf
// https://github.com/scala/scala/blob/724be0e9425b9ad07c244d25efdad695d75abbcf/src/compiler/scala/tools/nsc/backend/jvm/analysis/BackendUtils.scala#L790 
abstract class NestedClassesCollector[T](nestedOnly: Boolean) extends GenericSignatureVisitor(nestedOnly) {
  type InternalName = String 

  def declaredNestedClasses(internalName: InternalName): List[T]
  def getClassIfNested(internalName: InternalName): Option[T]
  
  val declaredInnerClasses = mutable.Set.empty[T]
  val referredInnerClasses = mutable.Set.empty[T]

  def innerClasses: collection.Set[T] = declaredInnerClasses ++ referredInnerClasses
  def clear(): Unit = {
    declaredInnerClasses.clear()
    referredInnerClasses.clear()
  }

  def visit(classNode: ClassNode): Unit = {
    visitInternalName(classNode.name)
    declaredInnerClasses ++= declaredNestedClasses(classNode.name)

    visitInternalName(classNode.superName)
    classNode.interfaces.asScala foreach visitInternalName
    visitInternalName(classNode.outerClass)

    visitAnnotations(classNode.visibleAnnotations)
    visitAnnotations(classNode.visibleTypeAnnotations)
    visitAnnotations(classNode.invisibleAnnotations)
    visitAnnotations(classNode.invisibleTypeAnnotations)

    visitClassSignature(classNode.signature)

    for (f <- classNode.fields.asScala) {
      visitDescriptor(f.desc)
      visitAnnotations(f.visibleAnnotations)
      visitAnnotations(f.visibleTypeAnnotations)
      visitAnnotations(f.invisibleAnnotations)
      visitAnnotations(f.invisibleTypeAnnotations)
      visitFieldSignature(f.signature)
    }

    for (m <- classNode.methods.asScala) {
      visitDescriptor(m.desc)

      visitAnnotations(m.visibleAnnotations)
      visitAnnotations(m.visibleTypeAnnotations)
      visitAnnotations(m.invisibleAnnotations)
      visitAnnotations(m.invisibleTypeAnnotations)
      visitAnnotationss(m.visibleParameterAnnotations)
      visitAnnotationss(m.invisibleParameterAnnotations)
      visitAnnotations(m.visibleLocalVariableAnnotations)
      visitAnnotations(m.invisibleLocalVariableAnnotations)

      m.exceptions.asScala foreach visitInternalName
      for (tcb <- m.tryCatchBlocks.asScala) visitInternalName(tcb.`type`)

      val iter = m.instructions.iterator
      while (iter.hasNext) iter.next() match {
        case ti: TypeInsnNode           => visitInternalNameOrArrayReference(ti.desc)
        case fi: FieldInsnNode          => visitInternalNameOrArrayReference(fi.owner); visitDescriptor(fi.desc)
        case mi: MethodInsnNode         => visitInternalNameOrArrayReference(mi.owner); visitDescriptor(mi.desc)
        case id: InvokeDynamicInsnNode  => visitDescriptor(id.desc); visitHandle(id.bsm); id.bsmArgs foreach visitConstant
        case ci: LdcInsnNode            => visitConstant(ci.cst)
        case ma: MultiANewArrayInsnNode => visitDescriptor(ma.desc)
        case _ =>
      }

      visitMethodSignature(m.signature)
    }
  }

  private def containsChar(s: String, offset: Int, length: Int, char: Char): Boolean = {
    val ix = s.indexOf(char, offset)
    !(ix == -1 || ix >= offset + length)
  }

  def visitInternalName(internalName: String, offset: Int, length: Int): Unit = if (internalName != null && containsChar(internalName, offset, length, '$')) {
    for (c <- getClassIfNested(internalName.substring(offset, length)))
      if (!declaredInnerClasses.contains(c))
        referredInnerClasses += c
  }

  // either an internal/Name or [[Linternal/Name; -- there are certain references in classfiles
  // that are either an internal name (without the surrounding `L;`) or an array descriptor
  // `[Linternal/Name;`.
  def visitInternalNameOrArrayReference(ref: String): Unit = if (ref != null) {
    val bracket = ref.lastIndexOf('[')
    if (bracket == -1) visitInternalName(ref)
    else if (ref.charAt(bracket + 1) == 'L') visitInternalName(ref, bracket + 2, ref.length - 1)
  }

  // we are only interested in the class references in the descriptor, so we can skip over
  // primitives and the brackets of array descriptors
  def visitDescriptor(desc: String): Unit = (desc.charAt(0): @switch) match {
    case '(' =>
      var i = 1
      while (i < desc.length) {
        if (desc.charAt(i) == 'L') {
          val start = i + 1 // skip the L
          var seenDollar = false
          while ({val ch = desc.charAt(i); seenDollar ||= (ch == '$'); ch != ';'}) i += 1
          if (seenDollar)
            visitInternalName(desc, start, i)
        }
        // skips over '[', ')', primitives
        i += 1
      }

    case 'L' =>
      visitInternalName(desc, 1, desc.length - 1)

    case '[' =>
      visitInternalNameOrArrayReference(desc)

    case _ => // skip over primitive types
  }

  def visitConstant(const: AnyRef): Unit = const match {
    case t: Type => visitDescriptor(t.getDescriptor)
    case _ =>
  }

  // in principle we could references to annotation types, as they only end up as strings in the
  // constant pool, not as class references. however, the java compiler still includes nested
  // annotation classes in the innerClass table, so we do the same. explained in detail in the
  // large comment in class BTypes.
  def visitAnnotation(annot: AnnotationNode): Unit = {
    visitDescriptor(annot.desc)
    if (annot.values != null) annot.values.asScala foreach visitConstant
  }

  def visitAnnotations(annots: java.util.List[_ <: AnnotationNode]) = if (annots != null) annots.asScala foreach visitAnnotation
  def visitAnnotationss(annotss: Array[java.util.List[AnnotationNode]]) = if (annotss != null) annotss foreach visitAnnotations

  def visitHandle(handle: Handle): Unit = {
    visitInternalNameOrArrayReference(handle.getOwner)
    visitDescriptor(handle.getDesc)
  }
}

