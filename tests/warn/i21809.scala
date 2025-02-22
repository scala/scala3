//> using options -Wunused:imports

package p {
  package q {
    import q.* // warn so long as we pass typer
    class Test {
      //override def toString = new C().toString + " for Test"
      def d = D()
    }
    class D
  }
}
package q {
  class C {
    override def toString = "q.C"
  }
}
