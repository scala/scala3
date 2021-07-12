import annotation.nowarn

@deprecated def f = 1

def t1 = f // error

@nowarn("cat=deprecation") def t2 = f
@nowarn("msg=deprecated") def t3 = f
@nowarn("msg=fish") def t4 = f // error
@nowarn("") def t5 = f
@nowarn def t6 = f

def t7 = f: @nowarn("cat=deprecation")
def t8 = f: @nowarn("msg=deprecated")
def t9 = f: @nowarn("msg=fish") // error
def t10 = f: @nowarn("")
def t11 = f: @nowarn
