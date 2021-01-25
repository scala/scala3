trait C:
  inline def x: Int
  inline val y: Int

class C1 extends C:
  inline val x = 1
  inline val y = 2

class C2 extends C:
  inline def x = 3
  inline val y = 4
