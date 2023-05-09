
package p:
  trait T:
    def t(i: Int) = i + 1
    //def t(s: String) = s + "_1" // it won't try to reconcile the overloaded member

  package object q extends T

  package q:

    class C extends T:
      def c = t(42) // should error because this.t is not q.t, but is currently reconciled
