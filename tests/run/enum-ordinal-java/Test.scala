enum Color1 extends Lib1.MyJavaEnum[Color1]:
  case Red, Green, Blue

enum Color2 extends Lib2.JavaEnumAlias[Color2]:
    case Red, Green, Blue

@main def Test =
  assert(Color1.Green.ordinal == 1)
  assert(Color2.Blue.ordinal == 2)
