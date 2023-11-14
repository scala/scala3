/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2015, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala
package util

import java.io.{ IOException, PrintWriter }
import java.util.jar.Attributes.Name as AttributeName

private[scala] trait PropertiesTrait {
  protected def propCategory: String      // specializes the remainder of the values
  protected def pickJarBasedOn: Class[?]  // props file comes from jar containing this

  /** The name of the properties file */
  protected val propFilename = "/" + propCategory + ".properties"

  /** The loaded properties */
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

  final def propIsSet(name: String)                   = System.getProperty(name) != null
  final def propIsSetTo(name: String, value: String)  = propOrNull(name) == value
  final def propOrElse(name: String, alt: String)     = System.getProperty(name, alt)
  final def propOrEmpty(name: String)                 = propOrElse(name, "")
  final def propOrNull(name: String)                  = propOrElse(name, null)
  final def propOrNone(name: String)                  = Option(propOrNull(name))
  final def propOrFalse(name: String)                 = propOrNone(name) exists (x => List("yes", "on", "true") contains x.toLowerCase)
  final def setProp(name: String, value: String)      = System.setProperty(name, value)
  final def clearProp(name: String)                   = System.clearProperty(name)

  final def envOrElse(name: String, alt: String)      = Option(System.getenv(name)) getOrElse alt
  final def envOrNone(name: String)                   = Option(System.getenv(name))

  final def envOrSome(name: String, alt: Option[String])       = envOrNone(name) orElse alt

  // for values based on propFilename, falling back to System properties
  final def scalaPropOrElse(name: String, alt: String): String = scalaPropOrNone(name).getOrElse(alt)
  final def scalaPropOrEmpty(name: String): String             = scalaPropOrElse(name, "")
  final def scalaPropOrNone(name: String): Option[String]      = Option(scalaProps.getProperty(name)).orElse(propOrNone("scala." + name))

  /** The numeric portion of the runtime Scala version, if this is a final
   *  release.  If for instance the versionString says "version 2.9.0.final",
   *  this would return Some("2.9.0").
   *
   *  @return Some(version) if this is a final release build, None if
   *  it is an RC, Beta, etc. or was built from source, or if the version
   *  cannot be read.
   */
  val releaseVersion =
    for {
      v <- scalaPropOrNone("maven.version.number")
      if !(v.endsWith("-SNAPSHOT"))
    } yield v

  /** The development Scala version, if this is not a final release.
   *  The precise contents are not guaranteed, but it aims to provide a
   *  unique repository identifier (currently the svn revision) in the
   *  fourth dotted segment if the running version was built from source.
   *
   *  @return Some(version) if this is a non-final version, None if this
   *  is a final release or the version cannot be read.
   */
  val developmentVersion =
    for {
      v <- scalaPropOrNone("maven.version.number")
      if v.endsWith("-SNAPSHOT")
      ov <- scalaPropOrNone("version.number")
    } yield ov

  /** Either the development or release version if known, otherwise
   *  the empty string.
   */
  def versionNumberString = scalaPropOrEmpty("version.number")

  /** The version number of the jar this was loaded from plus "version " prefix,
   *  or "version (unknown)" if it cannot be determined.
   */
  val versionString         = "version " + scalaPropOrElse("version.number", "(unknown)")
  val copyrightString       = scalaPropOrElse("copyright.string", "Copyright 2002-2017, LAMP/EPFL and Lightbend, Inc.")
}
