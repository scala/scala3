/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package util

import scala.language.`2.13`
import java.io.{IOException, PrintWriter}
import java.util.jar.Attributes.{Name => AttributeName}
import scala.annotation.tailrec
import language.experimental.captureChecking

/** Loads `library.properties` from the jar. */
object Properties extends PropertiesTrait {
  /** The category of properties to load, used to construct the properties file name. */
  protected def propCategory = "library"
  /** The class used to determine which JAR contains the properties file. */
  protected def pickJarBasedOn: Class[Option[?]] = classOf[Option[?]]

  /** Scala manifest attributes.
   */
  val ScalaCompilerVersion = new AttributeName("Scala-Compiler-Version")
}

private[scala] trait PropertiesTrait {
  /** The category of properties to load, used to construct the properties file name. */
  protected def propCategory: String      // specializes the remainder of the values
  /** The class used to determine which JAR contains the properties file. */
  protected def pickJarBasedOn: Class[?]  // props file comes from jar containing this

  /** The name of the properties file. */
  protected val propFilename = "/" + propCategory + ".properties"

  /** The loaded properties. */
  protected lazy val scalaProps: java.util.Properties = {
    val props = new java.util.Properties
    val stream = pickJarBasedOn.getResourceAsStream(propFilename)
    if (stream ne null)
      quietlyDispose(props.load(stream), stream.close)

    props
  }

  private def quietlyDispose(action: => Unit, disposal: => Unit) =
    try     { action }
    finally {
        try     { disposal }
        catch   { case _: IOException => }
    }

  /** Returns whether the system property with the given name is set.
   *
   *  @param name the name of the system property to check
   */
  def propIsSet(name: String): Boolean                   = System.getProperty(name) != null
  /** Returns whether the system property with the given name is set to the given value.
   *
   *  @param name the name of the system property to check
   *  @param value the expected value of the system property
   */
  def propIsSetTo(name: String, value: String)           = propOrNull(name) == value
  /** Returns the system property with the given name as an `Option`.
   *
   *  @param name the name of the system property to retrieve
   */
  def propOrNone(name: String): Option[String]           = Option[String](System.getProperty(name))
  /** Returns the system property with the given name, or the given alternative if the property is not set.
   *
   *  @param name the name of the system property to retrieve
   *  @param alt the alternative value to return if the property is not set
   */
  def propOrElse(name: String, alt: => String): String   = propOrNone(name).getOrElse(alt)
  /** Returns the system property with the given name, or an empty string if the property is not set.
   *
   *  @param name the name of the system property to retrieve
   */
  def propOrEmpty(name: String): String                  = propOrElse(name, "")
  /** Returns the system property with the given name, or `null` if the property is not set.
   *
   *  @param name the name of the system property to retrieve
   */
  def propOrNull(name: String): String | Null            = propOrNone(name).orNull
  /** Returns whether the system property with the given name is set to
   *  `"yes"`, `"on"`, or `"true"`, compared case-insensitively.
   *
   *  @param name the name of the system property to check
   */
  def propOrFalse(name: String): Boolean                 = propOrNone(name) exists (x => List("yes", "on", "true") contains x.toLowerCase)
  /** Sets the system property with the given name to the given value.
   *
   *  @param name the name of the system property to set
   *  @param value the value to set the system property to
   *  @return the previous value of the property, or `null` if it was not set
   */
  def setProp(name: String, value: String): String       = System.setProperty(name, value)
  /** Clears the system property with the given name.
   *
   *  @param name the name of the system property to clear
   *  @return the previous value of the property, or `null` if it was not set
   */
  def clearProp(name: String): String                    = System.clearProperty(name)

  /** Returns the environment variable with the given name, or the given alternative if the variable is not set.
   *
   *  @param name the name of the environment variable to retrieve
   *  @param alt the alternative value to return if the variable is not set
   */
  def envOrElse(name: String, alt: => String): String    = Option(System.getenv(name)) getOrElse alt
  /** Returns the environment variable with the given name as an `Option`.
   *
   *  @param name the name of the environment variable to retrieve
   */
  def envOrNone(name: String): Option[String]            = Option(System.getenv(name))

  /** Returns the environment variable with the given name as an `Option`,
   *  or the given alternative `Option` if the variable is not set.
   *
   *  @param name the name of the environment variable to retrieve
   *  @param alt the alternative `Option` to return if the variable is not set
   */
  def envOrSome(name: String, alt: => Option[String])    = envOrNone(name) orElse alt

  // for values based on propFilename, falling back to System properties
  /** Returns the Scala property with the given name, or the given alternative if the property is not set.
   *
   *  @param name the name of the Scala property to retrieve
   *  @param alt the alternative value to return if the property is not set
   */
  def scalaPropOrElse(name: String, alt: => String): String = scalaPropOrNone(name).getOrElse(alt)
  /** Returns the Scala property with the given name, or an empty string if the property is not set.
   *
   *  @param name the name of the Scala property to retrieve
   */
  def scalaPropOrEmpty(name: String): String             = scalaPropOrElse(name, "")
  /** Returns the Scala property with the given name as an `Option`,
   *  looking first in the loaded properties file, then among the system
   *  properties with the name prefixed by `scala.`.
   *
   *  @param name the name of the Scala property to retrieve
   */
  def scalaPropOrNone(name: String): Option[String]      = Option(scalaProps.getProperty(name)).orElse(propOrNone("scala." + name))

  /** The version of the Scala runtime, if this is not a snapshot.
   */
  val releaseVersion = scalaPropOrNone("maven.version.number").filterNot(_.endsWith("-SNAPSHOT"))

  /** The version of the Scala runtime, if this is a snapshot.
   */
  val developmentVersion = scalaPropOrNone("maven.version.number").filter(_.endsWith("-SNAPSHOT")).flatMap(_ => scalaPropOrNone("version.number"))

  /** The version of the Scala runtime, or the empty string if unknown.
   *
   *  Note that the version of the Scala library need not correlate with the version of the Scala compiler
   *  used to emit either the library or user code.
   *
   *  For example, Scala 3.0 and 3.1 use the Scala 2.13 library, which is reflected in this version string.
   *  For the Dotty version, see `dotty.tools.dotc.config.Properties.versionNumberString`.
   */
  def versionNumberString = scalaPropOrEmpty("version.number")

  /** A verbose alternative to [[versionNumberString]].
   */
  val versionString         = s"version ${scalaPropOrElse("version.number", "(unknown)")}"
  /** The copyright string for the Scala runtime. */
  val copyrightString       = scalaPropOrElse("copyright.string", "Copyright 2002-2025, LAMP/EPFL and Lightbend, Inc. dba Akka")

  /** This is the encoding to use reading in source files, overridden with -encoding.
   *  Note that it uses "prop" i.e. looks in the scala jar, not the system properties.
   */
  def sourceEncoding        = scalaPropOrElse("file.encoding", "UTF-8")
  /** The class name of the default source reader. */
  def sourceReader          = scalaPropOrElse("source.reader", "scala.tools.nsc.io.SourceReader")

  /** This is the default text encoding, overridden (unreliably) with
   *  `JAVA_OPTS="-Dfile.encoding=Foo"`
   */
  def encodingString        = propOrElse("file.encoding", "UTF-8")

  /** The default end of line character.
   */
  def lineSeparator: String = System.lineSeparator()

  /* Various well-known properties. */
  /** The Java class path. */
  def javaClassPath         = propOrEmpty("java.class.path")
  /** The Java home directory. */
  def javaHome              = propOrEmpty("java.home")
  /** The Java vendor. */
  def javaVendor            = propOrEmpty("java.vendor")
  /** The Java version. */
  def javaVersion           = propOrEmpty("java.version")
  /** The Java VM info. */
  def javaVmInfo            = propOrEmpty("java.vm.info")
  /** The Java VM name. */
  def javaVmName            = propOrEmpty("java.vm.name")
  /** The Java VM vendor. */
  def javaVmVendor          = propOrEmpty("java.vm.vendor")
  /** The Java VM version. */
  def javaVmVersion         = propOrEmpty("java.vm.version")
  /** The Java specification version. */
  def javaSpecVersion       = propOrEmpty("java.specification.version")
  /** The Java specification vendor. */
  def javaSpecVendor        = propOrEmpty("java.specification.vendor")
  /** The Java specification name. */
  def javaSpecName          = propOrEmpty("java.specification.name")
  /** The operating system name. */
  def osName                = propOrEmpty("os.name")
  /** The Scala home directory. */
  def scalaHome             = propOrEmpty("scala.home")
  /** The temporary directory. */
  def tmpDir                = propOrEmpty("java.io.tmpdir")
  /** The user's current working directory. */
  def userDir               = propOrEmpty("user.dir")
  /** The user's home directory. */
  def userHome              = propOrEmpty("user.home")
  /** The user's name. */
  def userName              = propOrEmpty("user.name")

  /* Some derived values. */
  /** Returns `true` iff the underlying operating system is a version of Microsoft Windows. */
  lazy val isWin            = osName.startsWith("Windows")
  // See https://mail.openjdk.java.net/pipermail/macosx-port-dev/2012-November/005148.html for
  // the reason why we don't follow developer.apple.com/library/mac/#technotes/tn2002/tn2110.
  /** Returns `true` iff the underlying operating system is a version of Apple Mac OSX. */
  lazy val isMac            = osName.startsWith("Mac OS X")
  /** Returns `true` iff the underlying operating system is a Linux distribution. */
  lazy val isLinux          = osName.startsWith("Linux")

  /* Some runtime values. */
  private[scala] lazy val isAvian = javaVmName.contains("Avian")

  private[scala] def coloredOutputEnabled: Boolean = propOrElse("scala.color", "auto") match {
    case "auto" => consoleIsTerminal
    case s      => "" == s || "true".equalsIgnoreCase(s)
  }

  /** System.console.isTerminal, or just check for null console on JDK < 22. */
  private[scala] lazy val consoleIsTerminal: Boolean = {
    import scala.reflect.Selectable.reflectiveSelectable
    val console = System.console
    def isTerminal: Boolean =
      try console.asInstanceOf[{ def isTerminal(): Boolean }].isTerminal()
      catch { case _: NoSuchMethodException => false }
    console != null && (!isJavaAtLeast("22") || isTerminal)
  }

  // This is looking for javac, tools.jar, etc.
  // Tries JDK_HOME first, then the more common but likely jre JAVA_HOME,
  // and finally the system property based javaHome.
  /** The JDK home directory, determined by checking the `JDK_HOME` environment variable, then `JAVA_HOME`, and finally the `java.home` system property. */
  def jdkHome               = envOrElse("JDK_HOME", envOrElse("JAVA_HOME", javaHome))

  private[scala] def versionFor(command: String) = s"Scala $command $versionString -- $copyrightString"

  /** The version message for the Scala runtime. */
  def versionMsg            = versionFor(propCategory)
  /** The name of the Scala command, depending on the operating system. */
  def scalaCmd              = if (isWin) "scala.bat" else "scala"
  /** The name of the Scala compiler command, depending on the operating system. */
  def scalacCmd             = if (isWin) "scalac.bat" else "scalac"

  /** Compares the given specification version to the specification version of the platform.
   *
   *  @param version a specification version number (legacy forms acceptable)
   *  @return `true` if the specification version of the current runtime
   *    is equal to or higher than the version denoted by the given string.
   *  @throws NumberFormatException if the given string is not a version string
   *
   *  @example ```
   *  // In this example, the runtime's Java specification is assumed to be at version 8.
   *  isJavaAtLeast("1.8")            // true
   *  isJavaAtLeast("8")              // true
   *  isJavaAtLeast("9")              // false
   *  isJavaAtLeast("9.1")            // false
   *  isJavaAtLeast("1.9")            // throws
   *  ```
   */
  def isJavaAtLeast(version: String): Boolean = {
    def versionOf(s: String, depth: Int): (Int, String) =
      s.indexOf('.') match {
        case 0 =>
          (-2, s.substring(1))
        case 1 if depth == 0 && s.charAt(0) == '1' =>
          val r0 = s.substring(2)
          val (v, r) = versionOf(r0, 1)
          val n = if (v > 8 || r0.isEmpty) -2 else v   // accept 1.8, not 1.9 or 1.
          (n, r)
        case -1 =>
          val n = if (!s.isEmpty) s.toInt else if (depth == 0) -2 else 0
          (n, "")
        case i  =>
          val r = s.substring(i + 1)
          val n = if (depth < 2 && r.isEmpty) -2 else s.substring(0, i).toInt
          (n, r)
      }
    @tailrec
    def compareVersions(s: String, v: String, depth: Int): Int = {
      if (depth >= 3) 0
      else {
        val (sn, srest) = versionOf(s, depth)
        val (vn, vrest) = versionOf(v, depth)
        if (vn < 0) -2
        else if (sn < vn) -1
        else if (sn > vn) 1
        else compareVersions(srest, vrest, depth + 1)
      }
    }
    compareVersions(javaSpecVersion, version, 0) match {
      case -2 => throw new NumberFormatException(s"Not a version: $version")
      case i  => i >= 0
    }
  }

  /** Compares the given specification version to the major version of the platform.
   *
   *  @param version a specification major version number
   *  @return `true` if the specification version of the current runtime is equal to or higher than the given version
   */
  def isJavaAtLeast(version: Int): Boolean = isJavaAtLeast(math.max(version, 0).toString)

  // provide a main method so version info can be obtained by running this
  /** Prints the version message to the standard error stream.
   *
   *  @param args the command-line arguments (not used)
   */
  def main(args: Array[String]): Unit = {
    val writer = new PrintWriter(Console.err, true)
    writer.println(versionMsg)
  }
}
