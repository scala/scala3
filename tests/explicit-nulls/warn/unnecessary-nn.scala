def f1(s: String): String = s.nn // warn
def f2(s: String|Null): String|Null = s.nn // warn
def f3(s: String|Null): Any = s.nn // warn
def f4(s: String|Null): String = s.nn

def f5[T >: String](s: String|Null): T = s.nn
def f6[T >: String|Null](s: String|Null): T = s.nn // warn

def f5a[T <: String](s: T): String = s.nn // warn

// flexible types
def f7(s: String|Null): String = "".concat(s.nn) // warn
def f8(s: String): String = s.trim().nn // OK because the .nn could be useful as a dynamic null check


def f9(s: String|Null): String =
  if(s == null) "default"
  else s.nn // warn

def f10(x: String|Null): ((String|Null) @deprecated) = x.nn // warn
def f10b(x: String|Null): (String|Null) = x.nn // warn