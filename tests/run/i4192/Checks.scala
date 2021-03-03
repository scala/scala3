package checks

import reflect.ClassTag

/* This test checks whether InnerClasses and EnclosingMethod sections in generated class files are correct
 * for different possibilities of nesting of classes in other classes, objects and methods (the attributes are accessed via java reflection).
 * Names of nested definitions are derived from the name of their enclosing definition by appending a letter following the scheme below:
 * A - a class without a companion object
 * B - an object without a companion class
 * C - a class with its companion object
 * D - a method
 * Additionally a number may be added to avoid clashes between definitions from classes and their companion objects
 * (1 - defined in the companion class; 2 - defined in the companion object),
 * e.g. ACD2 - a method inside the companion object of a class inside a top level class
 */
trait Checks:
  val expectedTopLevelChecksCount: Int
  val expectedMemberChecksCount: Int
  val expectedLocalChecksCount: Int

  var topLevelChecksCount = 0
  var memberChecksCount = 0
  var localChecksCount = 0

  def verifyChecksCounts() =
    assert(topLevelChecksCount == expectedTopLevelChecksCount,
      s"top level checks: expected $expectedTopLevelChecksCount but was $topLevelChecksCount")
    assert(memberChecksCount == expectedMemberChecksCount,
      s"member checks: expected $expectedMemberChecksCount but was $memberChecksCount")
    assert(localChecksCount == expectedLocalChecksCount,
      s"local checks: expected $expectedLocalChecksCount but was $localChecksCount")

  /** Check JVM class properties of a top level class */
  def checkTopLevel[ThisClass](using thisTag: ClassTag[ThisClass]) =
    val cls = thisTag.runtimeClass
    assert(cls.getEnclosingClass == null, s"Top level class $cls should have no enclosing class")
    assert(cls.getDeclaringClass == null, s"Top level class $cls should have no declaring class")
    assert(cls.getEnclosingMethod == null, s"Top level class $cls should have no enclosing method")
    topLevelChecksCount += 1

  /** Check JVM class properties of a member class (defined directly inside another class) */
  def checkMember[ThisClass, EnclosingClass](using thisTag: ClassTag[ThisClass], enclosingTag: ClassTag[EnclosingClass]) =
    val cls = thisTag.runtimeClass
    def className = cls.simpleName
    def enclosingClassName = cls.getEnclosingClass.simpleName
    def declaringClassName = cls.getDeclaringClass.simpleName
    val expectedEnclosingClassName = enclosingTag.runtimeClass.simpleName match
      case "B$" => "B" // classes defined directly in top level objects should be moved to their companion/mirror classes
      case "C$" => "C"
      case name => name
    assert(cls.getEnclosingClass != null,
      s"Member class $className should have an enclosing class")
    assert(enclosingClassName == expectedEnclosingClassName,
      s"The enclosing class of class $className should be $expectedEnclosingClassName but was $enclosingClassName")
    assert(cls.getDeclaringClass == cls.getEnclosingClass,
      s"The declaring class of class $className should be the same as its enclosing class but was $declaringClassName")
    assert(cls.getEnclosingMethod == null,
      s"Member class $className should have no enclosing method")
    memberChecksCount += 1

  /** Check JVM class properties of a local class (defined directly inside a method) */
  def checkLocal[ThisClass, EnclosingClass](using thisTag: ClassTag[ThisClass], enclosingTag: ClassTag[EnclosingClass]) =
    val cls = thisTag.runtimeClass
    def className = cls.simpleName
    def enclosingClassName = cls.getEnclosingClass.simpleName
    def meth = cls.getEnclosingMethod
    val expectedEnclosingClassName = enclosingTag.runtimeClass.simpleName
    // extracting method name basing on the described naming convention
    // $1 gets added during lambdaLift in case of a method defined inside another method
    val expectedEnclosingMethodName =
      val prefix = className.init
      val suffix = if prefix.filter(_.isLetter).endsWith("DD") then "$1" else ""
      prefix ++ suffix
    assert(cls.getEnclosingClass != null,
      s"Local class $className should have an enclosing class")
    assert(enclosingClassName == expectedEnclosingClassName,
      s"The enclosing class of class $className should be $expectedEnclosingClassName but was $enclosingClassName")
    assert(cls.getDeclaringClass == null,
      s"Local class $className should have no declaring class")
    assert(meth != null,
      s"Local class $className should have an enclosing method")
    assert(meth.getName == expectedEnclosingMethodName,
      s"The enclosing method of class $className should be $expectedEnclosingMethodName but was ${meth.getName}")
    localChecksCount += 1

  extension (cls: Class[?])
    // java 8 implementation of cls.getSimpleName() is buggy - https://bugs.java.com/bugdatabase/view_bug.do?bug_id=8057919
    def simpleName = cls.getName
      .stripSuffix("$1").stripSuffix("$2")
      .split("\\.").last
      .split("\\$").last
