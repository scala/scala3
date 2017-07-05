// For some reason, if the imports are not scoped, only the first import error
// is reported

class A {
  import scala.languageFeature$experimental$._ // error
}

class B {
  import scala.language$Scala2$._ // error
}

class C {
 import scala.languageFeature$._ // error
}

