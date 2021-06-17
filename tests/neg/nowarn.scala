import annotation.nowarn

@deprecated def f = 1

def t1 = f // warn

@nowarn("cat=deprecation") def t2 = f
@nowarn("msg=deprecated") def t3 = f
@nowarn("msg=fish") def t4 = f // warn
@nowarn("") def t5 = f
@nowarn def t6 = f

def t7 = f: @nowarn("cat=deprecation")
def t8 = f: @nowarn("msg=deprecated")
def t9 = f: @nowarn("msg=fish") // warn
def t10 = f: @nowarn("")
def t11 = f: @nowarn

def t12 = List(List(1): _*) // warn -source:future-migration
@nowarn("msg=vararg splices") def t13 = List(List(1): _*)

class K { override def blup = 1 } // error
