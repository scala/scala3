package dotty.dokka

import org.jetbrains.dokka.plugability.DokkaContext
import org.jetbrains.dokka.transformers.documentation.DocumentableTransformer
import org.jetbrains.dokka.model._
import collection.JavaConverters
import collection.JavaConverters._
import org.jetbrains.dokka.plugability.DokkaContext
import org.jetbrains.dokka.model.properties._
import dotty.dokka.model.api.memberExt

import dotty.dokka.model._
import dotty.dokka.model.api._

object DeprecatedTransformer extends DocumentableTransformer:

  private def updateRecursively(member: Member, op: (Boolean, Member) => Member): Member = 
    member.withMembers(member.allMembers.map { case m: Member => updateRecursively(op(member.isDeprecated, m), op) })
  
  override def invoke(original: DModule, context: DokkaContext): DModule =
    def appendDeprecated(isParentDeprecated: Boolean, m: Member): Member =
      m.withDeprecated(isParentDeprecated || !m.annotations.filter(a => a.dri.getPackageName == "java.lang" && a.dri.getClassNames == "Deprecated" ).isEmpty)
  
    original.updatePackages(_.map(p => updateRecursively(appendDeprecated(false, p), appendDeprecated).asInstanceOf[DPackage]))

  
    
