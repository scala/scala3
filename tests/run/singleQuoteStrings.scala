import language.experimental.dedentedStringLiterals

@main def Test =

  val plain = '''
    hello
    world!
    '''
  println(s"[$plain]")

  val foo = 3.5

  val interpolated = s'''
    $foo is
    ${"three"}
    '''
  println(s"[$interpolated]")

  val extra = ''''
    this is a
    ''' string
    ''''
  println(s"[$extra]")

  val extraI = s''''
    this is a
    ''' string $foo
    ''''
  println(s"[$extraI]")

  val extraDouble = s""""
    this is a
    ''' string $foo
    """"
  println(s"[$extraDouble]")

  val empty = '''
    '''
  println(s"[$empty]")
