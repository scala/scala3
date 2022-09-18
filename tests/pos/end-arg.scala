
def f =
  Option("hello, world") orElse {
    val text = "goodbyte"
    val res = Some(text)
    res
  }
  end orElse

def g =
  Option("hello, world").orElse {
    val text = "goodbyte"
    val res = Some(text)
    res
  }
  end orElse
