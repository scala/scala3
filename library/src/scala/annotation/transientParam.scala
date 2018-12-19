/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.annotation

import scala.annotation.meta._

/** An annotation that goes on parameters of classes or traits. It asserts
 *  that the parameter is used only for initialization and is not kept in
 *  the class as a field. Violations of this assertion are flagged as
 *  compile errors. The annotatoon is particularly useful for implicit
 *  parameters since for these a textual scan is not sufficient to know
 *  where they are used.
 */
@param class transientParam extends scala.annotation.StaticAnnotation
