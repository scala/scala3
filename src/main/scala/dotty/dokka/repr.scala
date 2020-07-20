package dotty.dokka

case class DDNamed(name: String, dri: )

/** Representation of package */
case class DDPackage(name: String, comment: String) 

/** Representation of class */
case class DDClass(name: String, pck: String, myExtend: Option[] comment: String)

/** Represenation of compilation unit */
case class DDUnit(classes: Seq[DDClass], packages: Seq[DDPackage]):
  lazy val classesByPackage = classes.groupBy(_.pck)