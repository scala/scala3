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

package scala.reflect.macros
package internal

import scala.language.`2.13`

/** Links macro definitions with their implementation.
 *  This is necessary to preserve macro def -> macro impl links between compilation runs.
 *
 *  More precisely, after typechecking right-hand side of a macro def
 *  `typedMacroBody` slaps `macroImpl` annotation onto the macro def
 *  with the result of typechecking as a sole parameter.
 *
 *  As an unfortunate consequence, this annotation must be defined in scala-library.jar,
 *  because anyone (even those programmers who compile their programs with only scala-library on classpath)
 *  must be able to define macros.
 *
 *  To lessen the weirdness we define this annotation as `private[scala]`.
 *  It will not prevent pickling, but it will prevent application developers (and scaladocs) from seeing the annotation.
 */
private[scala] final class macroImpl(val referenceToMacroImpl: Any) extends scala.annotation.StaticAnnotation
