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

import scala.language.`2.13`
import scala.annotation.meta

object languageFeature {

  @meta.languageFeature("extension of type scala.Dynamic", enableRequired = true)
  /** Serves as the witness type for the [[scala.language.dynamics `dynamics`]] language feature, which permits subclassing [[scala.Dynamic]]. */
  sealed trait dynamics
  object dynamics extends dynamics

  @meta.languageFeature("postfix operator #", enableRequired = true)
  /** Serves as the witness type for the [[scala.language.postfixOps `postfixOps`]] language feature, which permits postfix operator notation `expr op`. */
  sealed trait postfixOps
  object postfixOps extends postfixOps

  @meta.languageFeature("reflective access of structural type member #", enableRequired = false)
  /** Serves as the witness type for the [[scala.language.reflectiveCalls `reflectiveCalls`]] language feature, a legacy Scala 2 feature that is no longer supported in Scala 3. */
  sealed trait reflectiveCalls
  object reflectiveCalls extends reflectiveCalls

  @meta.languageFeature("implicit conversion #", enableRequired = false)
  /** Serves as the witness type for the [[scala.language.implicitConversions `implicitConversions`]] language feature, which permits defining implicit conversion methods. */
  sealed trait implicitConversions
  object implicitConversions extends implicitConversions

  @deprecated("scala.language.higherKinds no longer needs to be imported explicitly", "2.13.1")
  @meta.languageFeature("higher-kinded type", enableRequired = false)
  /** Serves as the witness type for the deprecated `higherKinds` language feature, a legacy Scala 2 feature that is no longer supported in Scala 3, where higher-kinded types need no language import. */
  sealed trait higherKinds
  @deprecated("scala.language.higherKinds no longer needs to be imported explicitly", "2.13.1")
  object higherKinds extends higherKinds

  @meta.languageFeature("#, which cannot be expressed by wildcards,", enableRequired = false)
  /** Serves as the witness type for the [[scala.language.existentials `existentials`]] language feature, a legacy Scala 2 feature that is no longer supported in Scala 3. */
  sealed trait existentials
  object existentials extends existentials

  object experimental {
    @meta.languageFeature("macro definition", enableRequired = true)
    /** Serves as the witness type for the [[scala.language.experimental.macros `experimental.macros`]] language feature, which permits Scala 2-style `def ... = macro ...` definitions. */
    sealed trait macros
    object macros extends macros
  }
}

