@insertVal // error
def foo(): Unit = ()

def bar =
  @insertVal // error
  def foo(): Unit = ()
