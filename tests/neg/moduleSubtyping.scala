class C {

  object o {

    var a: C.this.o.type = ???
    var b: this.type = ???
    a = b   // OK
    b = a   // OK

    var c: Test.o.type = ???
    a = c   // error
    b = c   // error
    c = a   // error
    c = b   // error
  }

}

object Test extends C {



}
