object Bar {

  inline def myMacro(): Unit = ${ Foo.aMacroImplementation }

  myMacro() // error

}
