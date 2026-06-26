package dotty

import java.lang.reflect.Modifier
import scala.jdk.CollectionConverters.*
import scala.language.unsafeNulls
import scala.tools.asm
import scala.tools.asm.*
import scala.tools.asm.tree.*

sealed trait AsmNode[+T] {
  def node: T
  def access: Int
  def desc: String
  def name: String
  def signature: String
  def attrs: Vector[Attribute]
  def visibleAnnotations: Vector[AnnotationNode]
  def invisibleAnnotations: Vector[AnnotationNode]
  def characteristics: String = "%15s %-30s%s%s".format(name, desc, accessString, sigString)
  def erasedCharacteristics: String = "%15s %-30s%s".format(name, desc, accessString)

  private def accessString      = if (access == 0) "" else " " + Modifier.toString(access)
  private def sigString         = if (signature == null) "" else " " + signature
  override def toString: String = characteristics
}

object AsmNode {
  type AsmMethod = AsmNode[MethodNode]
  type AsmField = AsmNode[FieldNode]
  type AsmMember = AsmNode[?]

  implicit class ClassNodeOps(val node: ClassNode) {
    def fieldsAndMethods: Vector[AsmMember] = {
      val xs: Vector[AsmMember] =
           node.methods.asScala.toVector.map(x => x: AsmMethod)
        ++ node.fields.asScala.toVector.map(x => x: AsmField)
      xs sortBy (_.characteristics)
    }
  }
  implicit class AsmMethodNode(val node: MethodNode) extends AsmNode[MethodNode] {
    def access: Int                                = node.access
    def desc: String                               = node.desc
    def name: String                               = node.name
    def signature: String                          = node.signature
    def attrs: Vector[Attribute]                     = node.attrs.asScala.toVector
    def visibleAnnotations: Vector[AnnotationNode]   = node.visibleAnnotations.asScala.toVector
    def invisibleAnnotations: Vector[AnnotationNode] = node.invisibleAnnotations.asScala.toVector
  }
  implicit class AsmFieldNode(val node: FieldNode) extends AsmNode[FieldNode] {
    def access: Int                                = node.access
    def desc: String                               = node.desc
    def name: String                               = node.name
    def signature: String                          = node.signature
    def attrs: Vector[Attribute]                     = node.attrs.asScala.toVector
    def visibleAnnotations: Vector[AnnotationNode]   = node.visibleAnnotations.asScala.toVector
    def invisibleAnnotations: Vector[AnnotationNode] = node.invisibleAnnotations.asScala.toVector
  }

  def apply(node: MethodNode): AsmMethodNode = new AsmMethodNode(node)
  def apply(node: FieldNode): AsmFieldNode   = new AsmFieldNode(node)
}
