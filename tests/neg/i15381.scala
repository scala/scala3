// scalac: -Vprint:parser

case class $[A](value: A)

def g: Int = $        // error

/*
was:
  |             Found:    .type
  |             Required: Int
 */
