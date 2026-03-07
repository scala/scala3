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
package control

import scala.language.`2.13`
import scala.annotation.tailrec
import scala.reflect.{ClassTag, classTag}
import scala.language.implicitConversions

/** Classes representing the components of exception handling.
 *
 *  Each class is independently composable.
 *
 *  This class differs from [[scala.util.Try]] in that it focuses on composing exception handlers rather than
 *  composing behavior.  All behavior should be composed first and fed to a [[Catch]] object using one of the
 *  `opt`, `either` or `withTry` methods. Taken together the classes provide a DSL for composing catch and finally
 *  behaviors.
 *
 *  ### Examples
 *
 *  Creates a `Catch` which handles specified exceptions.
 *  ```
 *  import scala.util.control.Exception._
 *  import java.net._
 *
 *  val s = "https://www.scala-lang.org/"
 *
 *  // Some(https://www.scala-lang.org/)
 *  val x1: Option[URL] = catching(classOf[MalformedURLException]) opt new URL(s)
 *
 *  // Right(https://www.scala-lang.org/)
 *  val x2: Either[Throwable,URL] =
 *    catching(classOf[MalformedURLException], classOf[NullPointerException]) either new URL(s)
 *
 *  // Success(https://www.scala-lang.org/)
 *  val x3: Try[URL] = catching(classOf[MalformedURLException], classOf[NullPointerException]) withTry new URL(s)
 *
 *  val defaultUrl = new URL("http://example.com")
 *  //  URL(http://example.com) because htt/xx throws MalformedURLException
 *  val x4: URL = failAsValue(classOf[MalformedURLException])(defaultUrl)(new URL("htt/xx"))
 *  ```
 *
 *  Creates a `Catch` which logs exceptions using `handling` and `by`.
 *  ```
 *  def log(t: Throwable): Unit = t.printStackTrace
 *
 *  val withThrowableLogging: Catch[Unit] = handling(classOf[MalformedURLException]) by (log)
 *
 *  def printUrl(url: String) : Unit = {
 *    val con = new URL(url) openConnection()
 *    val source = scala.io.Source.fromInputStream(con.getInputStream())
 *    source.getLines().foreach(println)
 *  }
 *
 *  val badUrl = "htt/xx"
 *  // Prints stacktrace,
 *  //   java.net.MalformedURLException: no protocol: htt/xx
 *  //     at java.net.URL.<init>(URL.java:586)
 *  withThrowableLogging { printUrl(badUrl) }
 *
 *  val goodUrl = "https://www.scala-lang.org/"
 *  // Prints page content,
 *  //   &lt;!DOCTYPE html&gt;
 *  //   &lt;html&gt;
 *  withThrowableLogging { printUrl(goodUrl) }
 *  ```
 *
 *  Use `unwrapping` to create a `Catch` that unwraps exceptions before rethrowing.
 *  ```
 *  class AppException(cause: Throwable) extends RuntimeException(cause)
 *
 *  val unwrappingCatch: Catch[Nothing] = unwrapping(classOf[AppException])
 *
 *  def calcResult: Int = throw new AppException(new NullPointerException)
 *
 *  // Throws NPE not AppException,
 *  //   java.lang.NullPointerException
 *  //     at .calcResult(&lt;console&gt;:17)
 *  val result = unwrappingCatch(calcResult)
 *  ```
 *
 *  Use `failAsValue` to provide a default when a specified exception is caught.
 *
 *  ```
 *  val inputDefaulting: Catch[Int] = failAsValue(classOf[NumberFormatException])(0)
 *  val candidatePick = "seven" // scala.io.StdIn.readLine()
 *
 *  // Int = 0
 *  val pick = inputDefaulting(candidatePick.toInt)
 *  ```
 *
 *  Compose multiple `Catch`s with `or` to build a `Catch` that provides default values varied by exception.
 *  ```
 *  val formatDefaulting: Catch[Int] = failAsValue(classOf[NumberFormatException])(0)
 *  val nullDefaulting: Catch[Int] = failAsValue(classOf[NullPointerException])(-1)
 *  val otherDefaulting: Catch[Int] = nonFatalCatch withApply(_ => -100)
 *
 *  val combinedDefaulting: Catch[Int] = formatDefaulting or nullDefaulting or otherDefaulting
 *
 *  def p(s: String): Int = s.length * s.toInt
 *
 *  // Int = 0
 *  combinedDefaulting(p("tenty-nine"))
 *
 *  // Int = -1
 *  combinedDefaulting(p(null: String))
 *
 *  // Int = -100
 *  combinedDefaulting(throw new IllegalStateException)
 *
 *  // Int = 22
 *  combinedDefaulting(p("11"))
 *  ```
 *
 *  @groupname composition-catch Catch behavior composition
 *  @groupprio composition-catch 10
 *  @groupdesc composition-catch Build Catch objects from exception lists and catch logic
 *
 *  @groupname composition-finally Finally behavior composition
 *  @groupprio composition-finally 20
 *  @groupdesc composition-finally Build Catch objects from finally logic
 *
 *  @groupname canned-behavior General purpose catch objects
 *  @groupprio canned-behavior 30
 *  @groupdesc canned-behavior Catch objects with predefined behavior. Use combinator methods to compose additional behavior.
 *
 *  @groupname dsl DSL behavior composition
 *  @groupprio dsl 40
 *  @groupdesc dsl Expressive Catch behavior composition
 *
 *  @groupname composition-catch-promiscuously Promiscuous Catch behaviors
 *  @groupprio composition-catch-promiscuously 50
 *  @groupdesc composition-catch-promiscuously Useful if catching `ControlThrowable` or `InterruptedException` is required.
 *
 *  @groupname logic-container Logic Containers
 *  @groupprio logic-container 60
 *  @groupdesc logic-container Containers for catch and finally behavior.
 *
 *  @define protectedExceptions `ControlThrowable` or `InterruptedException`
 */

object Exception {
  type Catcher[+T] = PartialFunction[Throwable, T]

  def mkCatcher[Ex <: Throwable: ClassTag, T](isDef: Ex => Boolean, f: Ex => T): PartialFunction[Throwable, T] = new Catcher[T] {
    private def downcast(x: Throwable): Option[Ex] =
      if (classTag[Ex].runtimeClass.isAssignableFrom(x.getClass)) Some(x.asInstanceOf[Ex])
      else None

    def isDefinedAt(x: Throwable): Boolean = downcast(x) exists isDef
    def apply(x: Throwable): T = f(downcast(x).get)
  }

  def mkThrowableCatcher[T](isDef: Throwable => Boolean, f: Throwable => T): PartialFunction[Throwable, T] = mkCatcher[Throwable, T](isDef, f)

  implicit def throwableSubtypeToCatcher[Ex <: Throwable: ClassTag, T](pf: PartialFunction[Ex, T]): Catcher[T] =
    mkCatcher(pf.isDefinedAt, pf.apply)

  /** !!! Not at all sure of every factor which goes into this,
   *  and/or whether we need multiple standard variations.
   *  @param x the throwable to check
   *  @return true if `x` is a $protectedExceptions, otherwise false.
   */
  def shouldRethrow(x: Throwable): Boolean = x match {
    case _: ControlThrowable      => true
    case _: InterruptedException  => true
    // case _: java.lang.Error       => true ?
    case _                        => false
  }

  trait Described {
    protected val name: String
    private var _desc: String = ""
    def desc: String = _desc
    def withDesc(s: String): this.type = {
      _desc = s
      this
    }
    override def toString(): String = name + "(" + desc + ")"
  }

  /** A container class for finally code.
   *  @group logic-container
   */
  class Finally private[Exception](body: => Unit) extends Described {
    protected val name = "Finally"

    def and(other: => Unit): Finally = new Finally({ body ; other })
    def invoke(): Unit = { body }
  }

  /** A container class for catch/finally logic.
   *
   *  Pass a different value for rethrow if you want to probably
   *  unwisely allow catching control exceptions and other throwables
   *  which the rest of the world may expect to get through.
   *  @tparam T result type produced by the catch logic
   *  @param pf partial function used when applying catch logic to determine result value
   *  @param fin finally logic which, if defined, will be invoked after catch logic
   *  @param rethrow predicate on throwables determining when to rethrow a caught [[Throwable]]
   *  @group logic-container
   */
  class Catch[+T](
    val pf: Catcher[T],
    val fin: Option[Finally] = None,
    val rethrow: Throwable => Boolean = shouldRethrow)
  extends Described {

    protected val name = "Catch"

    /** Creates a new Catch with additional exception handling logic.
     *
     *  @tparam U the result type of the combined catch logic, a supertype of `T`
     *  @param pf2 the additional exception handler to combine with the existing one
     */
    def or[U >: T](pf2: Catcher[U]): Catch[U] = new Catch(pf orElse pf2, fin, rethrow)
    def or[U >: T](other: Catch[U]): Catch[U] = or(other.pf)

    /** Applies this catch logic to the supplied body.
     *
     *  @tparam U the result type of the body, a supertype of `T`
     *  @param body the code block to execute with exception handling
     */
    def apply[U >: T](body: => U): U =
      try body
      catch {
        case x if rethrow(x)        => throw x
        case x if pf isDefinedAt x  => pf(x)
      }
      finally fin foreach (_.invoke())

    /** Creates a new Catch container from this object and the supplied finally body.
     *  @param body the additional logic to apply after all existing finally bodies
     */
    def andFinally(body: => Unit): Catch[T] = {
      val appendedFin = fin map(_ and body) getOrElse new Finally(body)
      new Catch(pf, Some(appendedFin), rethrow)
    }

    /** Applies this catch logic to the supplied body, mapping the result
     *  into `Option[T]` - `None` if any exception was caught, `Some(T)` otherwise.
     *
     *  @tparam U the result type of the body, a supertype of `T`
     *  @param body the code block to execute, whose result is wrapped in `Some` on success
     */
    def opt[U >: T](body: => U): Option[U] = toOption(Some(body))

    /** Applies this catch logic to the supplied body, mapping the result
     *  into `Either[Throwable, T]` - `Left(exception)` if an exception was caught,
     *  `Right(T)` otherwise.
     *
     *  @tparam U the result type of the body, a supertype of `T`
     *  @param body the code block to execute, whose result is wrapped in `Right` on success
     */
    def either[U >: T](body: => U): Either[Throwable, U] = toEither(Right(body))

    /** Applies this catch logic to the supplied body, mapping the result
     *  into `Try[T]` - `Failure` if an exception was caught, `Success(T)` otherwise.
     *
     *  @tparam U the result type of the body, a supertype of `T`
     *  @param body the code block to execute, whose result is wrapped in `Success` on success
     */
    def withTry[U >: T](body: => U): scala.util.Try[U] = toTry(Success(body))

    /** Creates a `Catch` object with the same `isDefinedAt` logic as this one,
     *  but with the supplied `apply` method replacing the current one. 
     *
     *  @tparam U the result type of the new exception handler
     *  @param f the function to apply to caught exceptions instead of the current handler
     */
    def withApply[U](f: Throwable => U): Catch[U] = {
      val pf2 = new Catcher[U] {
        def isDefinedAt(x: Throwable): Boolean = pf isDefinedAt x
        def apply(x: Throwable): U = f(x)
      }
      new Catch(pf2, fin, rethrow)
    }

    /** Convenience methods. */
    def toOption: Catch[Option[T]] = withApply(_ => None)
    def toEither: Catch[Either[Throwable, T]] = withApply(Left(_))
    def toTry: Catch[scala.util.Try[T]] = withApply(x => Failure(x))
  }

  final val nothingCatcher: Catcher[Nothing]  = mkThrowableCatcher(_ => false, throw _)
  final def nonFatalCatcher[T]: Catcher[T]    = mkThrowableCatcher({ case NonFatal(_) => true; case _ => false }, throw _)
  final def allCatcher[T]: Catcher[T]         = mkThrowableCatcher(_ => true, throw _)

  /** The empty `Catch` object.
   *  @group canned-behavior
   */
  final val noCatch: Catch[Nothing] = new Catch(nothingCatcher) withDesc "<nothing>"

  /** A `Catch` object which catches everything.
   *  @group canned-behavior
   *
   *  @tparam T the result type of the `Catch` body
   */
  final def allCatch[T]: Catch[T] = new Catch(allCatcher[T]) withDesc "<everything>"

  /** A `Catch` object which catches non-fatal exceptions.
   *  @group canned-behavior
   *
   *  @tparam T the result type of the `Catch` body
   */
  final def nonFatalCatch[T]: Catch[T] = new Catch(nonFatalCatcher[T]) withDesc "<non-fatal>"

  /** Creates a `Catch` object which will catch any of the supplied exceptions.
   *  Since the returned `Catch` object has no specific logic defined and will simply
   *  rethrow the exceptions it catches, you will typically want to call `opt`,
   *  `either` or `withTry` on the return value, or assign custom logic by calling "withApply".
   *
   *  Note that `Catch` objects automatically rethrow `ControlExceptions` and others
   *  which should only be caught in exceptional circumstances.  If you really want
   *  to catch exactly what you specify, use `catchingPromiscuously` instead.
   *  @group composition-catch
   *
   *  @tparam T the result type of the `Catch` body
   *  @param exceptions the exception classes to catch
   *  @return a `Catch` object that will catch the specified exceptions
   */
  def catching[T](exceptions: Class[?]*): Catch[T] =
    new Catch(pfFromExceptions(exceptions*)) withDesc (exceptions map (_.getName) mkString ", ")

  def catching[T](c: Catcher[T]): Catch[T] = new Catch(c)

  /** Creates a `Catch` object which will catch any of the supplied exceptions.
   *  Unlike "catching" which filters out those in shouldRethrow, this one will
   *  catch whatever you ask of it including $protectedExceptions.
   *  @group composition-catch-promiscuously
   *
   *  @tparam T the result type of the `Catch` body
   *  @param exceptions the exception classes to catch, including $protectedExceptions
   */
  def catchingPromiscuously[T](exceptions: Class[?]*): Catch[T] = catchingPromiscuously(pfFromExceptions(exceptions*))
  def catchingPromiscuously[T](c: Catcher[T]): Catch[T]         = new Catch(c, None, _ => false)

  /** Creates a `Catch` object which catches and ignores any of the supplied exceptions.
   *  @group composition-catch
   *
   *  @param exceptions the exception classes to catch and ignore
   */
  def ignoring(exceptions: Class[?]*): Catch[Unit] =
    catching(exceptions*) withApply (_ => ())

  /** Creates a `Catch` object which maps all the supplied exceptions to `None`.
   *  @group composition-catch
   *
   *  @tparam T the value type of the resulting `Option`
   *  @param exceptions the exception classes to catch, mapping them to `None`
   */
  def failing[T](exceptions: Class[?]*): Catch[Option[T]] =
    catching(exceptions*) withApply (_ => None)

  /** Creates a `Catch` object which maps all the supplied exceptions to the given value.
   *  @group composition-catch
   *
   *  @tparam T the result type of the `Catch` body and the default value
   *  @param exceptions the exception classes to catch
   *  @param value the default value to return when one of the specified exceptions is caught
   */
  def failAsValue[T](exceptions: Class[?]*)(value: => T): Catch[T] =
    catching(exceptions*) withApply (_ => value)

  class By[T,R](f: T => R) {
    def by(x: T): R = f(x)
  }

  /** Returns a partially constructed `Catch` object, which you must give
   *  an exception handler function as an argument to `by`.
   *  @example
   *  ```
   *   handling(classOf[MalformedURLException], classOf[NullPointerException]) by (_.printStackTrace)
   *  ```
   *  @group dsl
   *
   *  @tparam T the result type of the handler function passed to `by`
   *  @param exceptions the exception classes to catch
   */
  def handling[T](exceptions: Class[?]*): By[Throwable => T, Catch[T]] = {
    def fun(f: Throwable => T): Catch[T] = catching(exceptions*) withApply f
    new By[Throwable => T, Catch[T]](fun)
  }

  /** Returns a `Catch` object with no catch logic and the argument as the finally logic.
   *  @group composition-finally
   *
   *  @tparam T the result type of the `Catch` body
   *  @param body the finally logic to execute after the `Catch` body completes
   */
  def ultimately[T](body: => Unit): Catch[T] = noCatch andFinally body

  /** Creates a `Catch` object which unwraps any of the supplied exceptions.
   *  @group composition-catch
   *
   *  @tparam T the result type of the `Catch` body
   *  @param exceptions the wrapper exception classes to unwrap before rethrowing
   */
  def unwrapping[T](exceptions: Class[?]*): Catch[T] = {
    @tailrec
    def unwrap(x: Throwable): Throwable =
      if (wouldMatch(x, exceptions) && x.getCause != null) unwrap(x.getCause)
      else x

    catching(exceptions*) withApply (x => throw unwrap(x))
  }

  /** Private.
   *
   *  @param x the throwable to test against `classes`
   *  @param classes the exception classes to match against
   */
  private def wouldMatch(x: Throwable, classes: scala.collection.Seq[Class[?]]): Boolean =
    classes exists (_.isAssignableFrom(x.getClass))

  private def pfFromExceptions(exceptions: Class[?]*): PartialFunction[Throwable, Nothing] =
    { case x if wouldMatch(x, exceptions) => throw x }
}
