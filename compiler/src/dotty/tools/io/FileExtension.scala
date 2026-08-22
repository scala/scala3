package dotty.tools.io

enum FileExtension(val toLowerCase: String):
  case Tasty extends FileExtension("tasty")
  case Betasty extends FileExtension("betasty")
  case Class extends FileExtension("class")
  case Jar extends FileExtension("jar")
  case Scala extends FileExtension("scala")
  case ScalaScript extends FileExtension("sc")
  case Java extends FileExtension("java")
  case Zip extends FileExtension("zip")
  case Inc extends FileExtension("inc")
  case Empty extends FileExtension("")

  /** Fallback extension */
  private case External(override val toLowerCase: String) extends FileExtension(toLowerCase)

  // The comparisons below use `eq` rather than `==`: all the compared cases are
  // singletons without `equals` overrides, and the only equals-overriding case
  // `External` can never equal a singleton case, so `==` and `eq` agree while
  // `eq` avoids the virtual `equals` dispatch on this very hot path.

  /** represents an empty file extension. */
  def isEmpty: Boolean = this eq Empty

  /** the full extension including a leading dot */
  val withDot: String = "." + toLowerCase

  override def toString: String = toLowerCase

  /** represents `".tasty"` */
  def isTasty = this eq Tasty
  /** represents `".betasty"` */
  def isBetasty = this eq Betasty
  /** represents `".class"` */
  def isClass = this eq Class
  /** represents `".scala"` */
  def isScala = this eq Scala
  /** represents `".sc"` */
  def isScalaScript = this eq ScalaScript
  /** represents `".java"` */
  def isJava = this eq Java
  /** represents `".jar"` */
  def isJar: Boolean = this eq Jar
  /** represents `".zip"` */
  def isZip: Boolean = this eq Zip
  /** represents `".jar"` or `".zip"` */
  def isJarOrZip: Boolean = isJar || isZip
  /** represents `".scala"` or `".java"` */
  def isSourceExtension: Boolean = isScala || isJava
  /** represents `".class"` or `".tasty"` */
  def isScalaBinary: Boolean = isClass || isTasty

object FileExtension:

  // this will be optimised to a single hashcode + equality check, and then fallback to slowLookup,
  // keep in sync with slowLookup.
  private def initialLookup(s: String): FileExtension = s match
    case "tasty" => Tasty
    case "class" => Class
    case "jar" => Jar
    case "scala" => Scala
    case "sc" => ScalaScript
    case "java" => Java
    case "zip" => Zip
    case "inc" => Inc
    case "betasty" => Betasty
    case _ => slowLookup(s)

  // slower than initialLookup, keep in sync with initialLookup
  private def slowLookup(s: String): FileExtension =
    if s.equalsIgnoreCase("tasty") then Tasty
    else if s.equalsIgnoreCase("class") then Class
    else if s.equalsIgnoreCase("jar") then Jar
    else if s.equalsIgnoreCase("scala") then Scala
    else if s.equalsIgnoreCase("sc") then ScalaScript
    else if s.equalsIgnoreCase("java") then Java
    else if s.equalsIgnoreCase("zip") then Zip
    else if s.equalsIgnoreCase("inc") then Inc
    else if s.equalsIgnoreCase("betasty") then Betasty
    else External(s)

  def from(s: String): FileExtension =
    if s.isEmpty then Empty
    else initialLookup(s)
