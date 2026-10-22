def inferredFromJava =
  val d = java.time.LocalDate.now() // flexible type (java.time.LocalDate)?
  d.synchronized {} // warn

def directlyOnJavaResult =
  java.time.LocalDate.now().synchronized {} // warn

def afterNn =
  val d: java.time.LocalDate = java.time.LocalDate.now().nn
  d.synchronized {} // warn
