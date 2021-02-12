def iarr = IArray(
  IArray(1, 2, 3),
  IArray(4, 5, 6),
  IArray(7, 8, 9)
)
def arr = Array( // same issue
  IArray(1, 2, 3),
  Array(4, 5, 6),
  Array(7, 8, 9)
)
