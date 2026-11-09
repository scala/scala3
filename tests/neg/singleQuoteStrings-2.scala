import language.experimental.dedentedStringLiterals

@main def Test =

  val a = '''     '''  // error // error

  val b = '''     !   // error
    '''

  val c = '''
      foo
    moo          // error
      '''

  val d = '''
      foo
    !'''           // error

  val e = '''     ${"hi"} '''  // error // error

  val f = ''''     !   // error
    $e
    ''''

  val g = '''
      foo
    $e               // error
      '''

  val h = '''
      $g
    !'''         // error

