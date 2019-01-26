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

package scala

/** A class for Product0 which was missing from the scala distribution. */
object Product0 {
  def unapply(x: Product0): Option[Product0] =
    Some(x)
}

trait Product0 extends Any with Product {

  override def productArity = 0

  @throws(classOf[IndexOutOfBoundsException])
  override def productElement(n: Int) =
    throw new IndexOutOfBoundsException(n.toString())
}
