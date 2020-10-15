package dotty.dokka.model

import org.jetbrains.dokka.model._
import collection.JavaConverters._

extension (m: DModule):
  def updatePackanges(op: Seq[DPackage] => Seq[DPackage]): DModule = 
    m.copy(
            m.getName,
            op(m.getPackages.asScala.toSeq).asJava,
            m.getDocumentation,
            m.getExpectPresentInSet,
            m.getSourceSets,
            m.getExtra
        )

extension (p: DPackage):
  def updateClasslikes(op: Seq[DClasslike] => Seq[DClasslike]): DPackage = 
    p.copy(
            p.getDri,
            p.getFunctions,
            p.getProperties,
            op(p.getClasslikes.asScala.toSeq).asJava,
            p.getTypealiases,
            p.getDocumentation,
            p.getExpectPresentInSet,
            p.getSourceSets,
            p.getExtra
        )

extension (c: DClass):
   def updateClasslikes(op: Seq[DClasslike] => Seq[DClasslike]): DClass =  
    c.copy(
      c.getDri,
      c.getName,
      c.getConstructors,
      c.getFunctions,
      c.getProperties,
      op(c.getClasslikes.asScala.toSeq).asJava,
      c.getSources,
      c.getVisibility,
      c.getCompanion,
      c.getGenerics,
      c.getSupertypes,
      c.getDocumentation,
      c.getExpectPresentInSet,
      c.getModifier,
      c.getSourceSets,
      c.getExtra
  )       
