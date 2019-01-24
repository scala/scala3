package logs

import Predef.{any2stringadd => _, _}

object Test {
  val l = Logarithm(1.0)
  val l2 = Logarithm(2.0)
  val l3 = l * l2
  val l4 = l + l2  // currently requires any2stringadd to be disabled because
                   // as a contextual implicit this takes precedence over the
                   // implicit scope implicit LogarithmOps.
                   // TODO: Remove any2stringadd
  val d = Logarithm.toDouble(l3)
  val l5: Logarithm = (1.0).asInstanceOf[Logarithm]
}
