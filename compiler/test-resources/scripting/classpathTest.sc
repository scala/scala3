#!dist/target/pack/bin/scala -classpath "dist/target/pack/lib/*"
!#

/*
 * a single wildcard -classpath entry is vulnerable to being converted by Windows jdk
 * to a list of jar files.  To prevent this Windows-only behavior, a semicolon is
 * added to the classpath entry by dist/bin/scala to prevent globbing.
 *
 * The symptom of failure is that args contains lots of jar file paths.
 */
import java.nio.file.Paths

// expecting classpath to contain scala compiler lib jars
def main(args: Array[String]): Unit =
  assert(args.isEmpty,s"args contains ${args.length} arguments, but was expecting none")

  val expected = Paths.get("dist/target/pack/lib").toFile.listFiles.toList.map { _.getName }.filter { _.endsWith(".jar") }
  def psep = java.io.File.pathSeparator
  val classpath = sys.props("java.class.path")
  val entries = classpath.split(psep).map { Paths.get(_).toFile.getName }

  val expectSet = expected.toSet
  val entrySet = entries.toSet

  if ((expectSet intersect entrySet).isEmpty) then
    for entry <- expected do
      printf("expect:%s\n",entry)
    for entry <- entries do
      printf("found:%s\n",entry)

  assert ((expectSet intersect entrySet).nonEmpty, s"expected[${expected.size}] but found [${entries.size}]")

  printf("success!\n")
