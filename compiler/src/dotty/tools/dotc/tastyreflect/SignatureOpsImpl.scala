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

package dotty.tools.dotc.tastyreflect

trait SignatureOpsImpl extends scala.tasty.reflect.SignatureOps with CoreImpl {

  object Signature extends SignatureModule {
    def unapply(x: Signature)(implicit ctx: Context): Option[(List[String], String)] = {
      Some((x.paramsSig.map(_.toString), x.resSig.toString))
    }
  }

  def SignatureDeco(sig: Signature): SignatureAPI = new SignatureAPI {
    def paramSigs: List[String] = sig.paramsSig.map(_.toString)
    def resultSig: String = sig.resSig.toString
  }

}
