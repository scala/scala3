package dotty.dokka.model.api

import org.jetbrains.dokka.model.DModule
import scala.jdk.CollectionConverters.{ListHasAsScala, SeqHasAsJava}
import dotty.dokka.model.api._

extension (m: DModule)
    def visitMembers(callback: Member => Unit): Unit = 
        def visitClasslike(c: Member, callback: Member => Unit): Unit =
            callback(c)
            c.allMembers.foreach(visitClasslike(_, callback))
        m.getPackages.asScala.foreach(_.allMembers.foreach(visitClasslike(_, callback)))

extension (s: Signature)
    def getName: String =
        s.map { _ match
            case s: String => s
            case l: Link => l.name
        }.mkString 

extension (m: Member):
    def getDirectParentsAsStrings: Seq[String] =
        m.directParents.map(_.getName).sorted
    def getParentsAsStrings: Seq[String] =
        m.parents.map(_.signature.getName).sorted
    def getKnownChildrenAsStrings: Seq[String] =   
        m.knownChildren.map(_.signature.getName).sorted
