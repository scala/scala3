package tests
package thisType

// issue 16024

class X[Map[_, _[_]]]:
  inline def map[F[_]](f: [t] => t => F[t]): Map[this.type, F] = //expected: inline def map[F[_]](f: [t] => (x$1: t) => F[t]): Map[this.type, F]
    ???
