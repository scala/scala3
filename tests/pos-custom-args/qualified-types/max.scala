def max(x: Int, y: Int): { v: Int with v >= x && v >= y } =
  if (x > y) x else y
