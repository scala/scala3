package o { class IO }
package p { class IO }
import o._
package q {
  import p._
  class D extends IO
}
