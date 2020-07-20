package dotty.dokka

/** Representation of package */
case class DDPackage(name: String, comment: String) 

/** Representation of class */
case class DDClass(name: String, pck: String, comment: String)

/** Represenation of compilation unit */
case class DDUnit(classes: Seq[DDClass], packages: Seq[DDPackage]):
  lazy val classesByPackage = classes.groupBy(_.pck)