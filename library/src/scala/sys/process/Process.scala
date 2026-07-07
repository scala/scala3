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
package sys
package process

import scala.language.`2.13`
import processInternal._
import ProcessBuilder._
import scala.language.implicitConversions


/** Represents a process that is running or has finished running.
 *  It may be a compound process with several underlying native processes (such as `a #&& b`).
 *
 *  This trait is often not used directly, though its companion object contains
 *  factories for [[scala.sys.process.ProcessBuilder]], the main component of this
 *  package.
 *
 *  It is used directly when calling the method `run` on a `ProcessBuilder`,
 *  which makes the process run in the background. The methods provided on `Process`
 *  make it possible for one to block until the process exits and get the exit value,
 *  or destroy the process altogether.
 *
 *  @see [[scala.sys.process.ProcessBuilder]]
 */
trait Process {
  /** Returns this process alive status. */
  def isAlive(): Boolean
  /** Blocks until this process exits and returns the exit code.*/
  def exitValue(): Int
  /** Destroys this process. */
  def destroy(): Unit
}

/** Methods for constructing simple commands that can then be combined. */
object Process extends ProcessImpl with ProcessCreation { }

/** Factories for creating [[scala.sys.process.ProcessBuilder]]. They can be
 *  found on and used through [[scala.sys.process.Process]]'s companion object.
 */
trait ProcessCreation {
  /** Creates a [[scala.sys.process.ProcessBuilder]] from a `String`, including the
   *  parameters.
   *
   *  @example
   *  ```
   *  apply("cat file.txt")
   *  ```
   *
   *  @param command the command string, including parameters separated by spaces
   *  @return a new `ProcessBuilder` for the given command
   */
  def apply(command: String): ProcessBuilder                         = apply(command, None)

  /** Creates a [[scala.sys.process.ProcessBuilder]] from a sequence of `String`,
   *  where the head is the command and each element of the tail is a parameter.
   *
   *  @example
   *  ```
   *  apply("cat" :: files)
   *  ```
   *
   *  @param command a sequence where the first element is the executable and the rest are arguments
   *  @return a new `ProcessBuilder` for the given command
   */
  def apply(command: scala.collection.Seq[String]): ProcessBuilder   = apply(command, None)

  /** Creates a [[scala.sys.process.ProcessBuilder]] from a command represented by a `String`,
   *  and a sequence of `String` representing the arguments.
   *
   *  @example
   *  ```
   *  apply("cat", files)
   *  ```
   *
   *  @param command the executable to run
   *  @param arguments the arguments to pass to the command
   *  @return a new `ProcessBuilder` for the given command and arguments
   */
  def apply(command: String, arguments: scala.collection.Seq[String]): ProcessBuilder = apply(command +: arguments, None)

  /** Creates a [[scala.sys.process.ProcessBuilder]] with working dir set to `File` and extra
   *  environment variables.
   *
   *  @example
   *  ```
   *  apply("java", new java.io.File("/opt/app"), "CLASSPATH" -> "library.jar")
   *  ```
   *
   *  @param command the command string, including parameters separated by spaces
   *  @param cwd the working directory for the process
   *  @param extraEnv environment variable name-value pairs to add to the process environment
   *  @return a new `ProcessBuilder` for the given command with the specified working directory
   */
  def apply(command: String, cwd: File, extraEnv: (String, String)*): ProcessBuilder =
    apply(command, Some(cwd), extraEnv*)

  /** Creates a [[scala.sys.process.ProcessBuilder]] with working dir set to `File` and extra
   *  environment variables.
   *
   *  @example
   *  ```
   *  apply("java" :: javaArgs, new java.io.File("/opt/app"), "CLASSPATH" -> "library.jar")
   *  ```
   *
   *  @param command a sequence where the first element is the executable and the rest are arguments
   *  @param cwd the working directory for the process
   *  @param extraEnv environment variable name-value pairs to add to the process environment
   *  @return a new `ProcessBuilder` for the given command with the specified working directory
   */
  def apply(command: scala.collection.Seq[String], cwd: File, extraEnv: (String, String)*): ProcessBuilder =
    apply(command, Some(cwd), extraEnv*)

  /** Creates a [[scala.sys.process.ProcessBuilder]] with working dir optionally set to
   *  `File` and extra environment variables.
   *
   *  @example
   *  ```
   *  apply("java", params.get("cwd"), "CLASSPATH" -> "library.jar")
   *  ```
   *
   *  @param command the command string, including parameters separated by spaces
   *  @param cwd an optional working directory for the process
   *  @param extraEnv environment variable name-value pairs to add to the process environment
   *  @return a new `ProcessBuilder` for the given command
   */
  def apply(command: String, cwd: Option[File], extraEnv: (String, String)*): ProcessBuilder =
    apply(Parser.tokenize(command), cwd, extraEnv*)

  /** Creates a [[scala.sys.process.ProcessBuilder]] with working dir optionally set to
   *  `File` and extra environment variables.
   *
   *  @example
   *  ```
   *  apply("java" :: javaArgs, params.get("cwd"), "CLASSPATH" -> "library.jar")
   *  ```
   *
   *  @param command a sequence where the first element is the executable and the rest are arguments
   *  @param cwd an optional working directory for the process
   *  @param extraEnv environment variable name-value pairs to add to the process environment
   *  @return a new `ProcessBuilder` for the given command
   */
  def apply(command: scala.collection.Seq[String], cwd: Option[File], extraEnv: (String, String)*): ProcessBuilder = {
    val jpb = new JProcessBuilder(command.toArray*)
    cwd foreach (jpb.directory(_))
    extraEnv foreach { case (k, v) => jpb.environment.put(k, v) }
    apply(jpb)
  }

  /** Creates a [[scala.sys.process.ProcessBuilder]] from a `java.lang.ProcessBuilder`.
   *
   *  @example ```
   *  apply((new java.lang.ProcessBuilder("ls", "-l")) directory new java.io.File(System.getProperty("user.home")))
   *  ```
   *
   *  @param builder the `java.lang.ProcessBuilder` to wrap
   *  @return a new `ProcessBuilder` wrapping the given Java process builder
   */
  def apply(builder: JProcessBuilder): ProcessBuilder = new Simple(builder)

  /** Creates a [[scala.sys.process.ProcessBuilder]] from a `java.io.File`. This
   *  `ProcessBuilder` can then be used as a `Source` or a `Sink`, so one can
   *  pipe things from and to it.
   *
   *  @param file the file to use as a source or sink for the process
   *  @return a `FileBuilder` wrapping the given file
   */
  def apply(file: File): FileBuilder                  = new FileImpl(file)

  /** Creates a [[scala.sys.process.ProcessBuilder]] from a `java.net.URL`. This
   *  `ProcessBuilder` can then be used as a `Source`, so that one can pipe things
   *  from it.
   *
   *  @param url the URL to use as a source for the process
   *  @return a `URLBuilder` wrapping the given URL
   */
  def apply(url: URL): URLBuilder                     = new URLImpl(url)

  /** Creates a [[scala.sys.process.ProcessBuilder]] from a `Boolean`. This can be
   *  to force an exit value.
   *
   *  @param value if `true`, the process exits with code 0; if `false`, with code 1
   *  @return a `ProcessBuilder` that immediately exits with the corresponding exit code
   */
  def apply(value: Boolean): ProcessBuilder           = apply(value.toString, if (value) 0 else 1)

  /** Creates a [[scala.sys.process.ProcessBuilder]] from a `String` name and a
   *  `Boolean`. This can be used to force an exit value, with the name being
   *  used for `toString`.
   *
   *  @param name the name used for the `toString` representation of this process
   *  @param exitValue the exit code that this process will return (by-name, evaluated on each access)
   *  @return a `ProcessBuilder` that immediately exits with the given exit code
   */
  def apply(name: String, exitValue: => Int): ProcessBuilder = new Dummy(name, exitValue)

  /** Creates a sequence of [[scala.sys.process.ProcessBuilder.Source]] from a sequence of
   *  something else for which there's an implicit conversion to `Source`.
   *
   *  @tparam T the type of the elements to be converted to `Source`
   *  @param builders the sequence of elements to convert
   *  @param convert the implicit conversion from `T` to `Source`
   *  @return a sequence of `Source` instances converted from the input elements
   */
  def applySeq[T](builders: scala.collection.Seq[T])(implicit convert: T => Source): scala.collection.Seq[Source] = builders.map(convert)

  /** Creates a [[scala.sys.process.ProcessBuilder]] from one or more
   *  [[scala.sys.process.ProcessBuilder.Source]], which can then be
   *  piped to something else.
   *
   *  This will concatenate the output of all sources. For example:
   *
   *  ```
   *  import scala.sys.process._
   *  import scala.sys.process.Process.cat
   *  import java.net.URL
   *  import java.io.File
   *
   *  val spde = new URL("http://technically.us/spde.html")
   *  val dispatch = new URL("https://dispatchhttp.org/Dispatch.html")
   *  val build = new File("project/build.properties")
   *  cat(spde, dispatch, build) #| "grep -i scala" !
   *  ```
   *
   *  @param file the first `Source` to concatenate
   *  @param files additional `Source` values to concatenate after the first
   *  @return a `ProcessBuilder` whose output is the concatenation of all sources
   */
  def cat(file: Source, files: Source*): ProcessBuilder = cat(file +: files)

  /** Creates a [[scala.sys.process.ProcessBuilder]] from a non-empty sequence
   *  of [[scala.sys.process.ProcessBuilder.Source]], which can then be
   *  piped to something else.
   *
   *  This will concatenate the output of all sources.
   *
   *  @param files the non-empty sequence of sources to concatenate; throws `IllegalArgumentException` if empty
   *  @return a `ProcessBuilder` whose output is the concatenation of all sources
   */
  def cat(files: scala.collection.Seq[Source]): ProcessBuilder = {
    require(files.nonEmpty)
    files.map(_.cat).reduceLeft(_ #&& _)
  }
}

/** Provides implicit conversions for the factories offered by [[scala.sys.process.Process]]'s
 *  companion object. These implicits can then be used to decrease the noise in a pipeline
 *  of commands, making it look more shell-like. They are available through the package object
 *  [[scala.sys.process]].
 */
trait ProcessImplicits {
  import Process._

  /** Returns a sequence of [[scala.sys.process.ProcessBuilder.Source]] from a sequence
   *  of values for which an implicit conversion to `Source` is available.
   *
   *  @tparam T the type of the elements to be converted to `Source`
   *  @param builders the sequence of elements to convert
   *  @param convert the implicit conversion from `T` to `Source`
   *  @return a sequence of `Source` instances converted from the input elements
   */
  implicit def buildersToProcess[T](builders: scala.collection.Seq[T])(implicit convert: T => Source): scala.collection.Seq[Source] = applySeq(builders)

  /** Implicitly convert a `java.lang.ProcessBuilder` into a Scala one.
   *
   *  @param builder the `java.lang.ProcessBuilder` to convert
   *  @return a Scala `ProcessBuilder` wrapping the given Java process builder
   */
  implicit def builderToProcess(builder: JProcessBuilder): ProcessBuilder = apply(builder)

  /** Implicitly convert a `java.io.File` into a
   *  [[scala.sys.process.ProcessBuilder.FileBuilder]], which can be used as
   *  either input or output of a process. For example:
   *  ```
   *  import scala.sys.process._
   *  "ls" #> new java.io.File("dirContents.txt") !
   *  ```
   *
   *  @param file the file to convert into a `FileBuilder`
   *  @return a `FileBuilder` wrapping the given file
   */
  implicit def fileToProcess(file: File): FileBuilder                     = apply(file)

  /** Implicitly convert a `java.net.URL` into a
   *  [[scala.sys.process.ProcessBuilder.URLBuilder]] , which can be used as
   *  input to a process. For example:
   *  ```
   *  import scala.sys.process._
   *  Seq("xmllint", "--html", "-") #< new java.net.URL("https://www.scala-lang.org") #> new java.io.File("fixed.html") !
   *  ```
   *
   *  @param url the URL to convert into a `URLBuilder`
   *  @return a `URLBuilder` wrapping the given URL
   */
  implicit def urlToProcess(url: URL): URLBuilder                         = apply(url)

  /** Implicitly convert a `String` into a [[scala.sys.process.ProcessBuilder]].
   *
   *  @param command the command string to convert into a `ProcessBuilder`
   *  @return a `ProcessBuilder` for the given command string
   */
  implicit def stringToProcess(command: String): ProcessBuilder           = apply(command)

  /** Implicitly convert a sequence of `String` into a
   *  [[scala.sys.process.ProcessBuilder]]. The first argument will be taken to
   *  be the command to be executed, and the remaining will be its arguments.
   *  When using this, arguments may contain spaces.
   *
   *  @param command a sequence where the first element is the executable and the rest are arguments
   *  @return a `ProcessBuilder` for the given command sequence
   */
  implicit def stringSeqToProcess(command: scala.collection.Seq[String]): ProcessBuilder = apply(command)
}
