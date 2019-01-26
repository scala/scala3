/*
 * Dotty (https://dotty.epfl.ch/)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tasty.reflect

trait SignatureOps extends Core {

  /** Erased (JVM) signatures. */
  val Signature: SignatureModule
  abstract class SignatureModule {
    /** Matches the erased (JVM) signature and returns its parameters and result type. */
    def unapply(sig: Signature)(implicit ctx: Context): Option[(List[String], String)]
  }

  trait SignatureAPI {
    /** The (JVM) erased signatures of the parameters. */
    def paramSigs: List[String]
    /** The (JVM) erased result type. */
    def resultSig: String
  }
  implicit def SignatureDeco(sig: Signature): SignatureAPI

}
