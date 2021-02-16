def a(o: String) = Seq(o, org)               // error: package org is not a value
def a2(o: String) = Seq[String](o, org)      // error: package org is not a value
def a3(o: String): Seq[String] = Seq(o, org) // error: package org is not a value
def a4(o: String): Seq[String] = Seq(o, org) // error: package org is not a value
