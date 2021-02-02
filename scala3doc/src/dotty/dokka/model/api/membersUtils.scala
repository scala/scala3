package dotty.dokka.model.api

extension (s: Signature)
  def getName: String =
    s.map {
      case s: String => s
      case l: Link => l.name
    }.mkString

extension (m: Member)
  def getDirectParentsAsStrings: Seq[String] =
    m.directParents.map(_.signature.getName).sorted
  def getParentsAsStrings: Seq[String] =
    m.parents.map(_.signature.getName).sorted
  def getKnownChildrenAsStrings: Seq[String] =
    m.knownChildren.map(_.signature.getName).sorted
