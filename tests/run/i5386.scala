object Test extends App {

  +{
    println("!")
    1
  }

  -{
    println("!")
    1
  }

  !{
    println("!")
    true
  }

  { println("1"); 1 } + { println("2"); 2}

  !(try true finally{()})


  class C {
    def foo: 1 = { println("1"); 1 }
  }

  { println("!"); new C }.foo

}