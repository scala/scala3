def escape(y: File^): File =
  Lib.mk(y).value((x: File) => x) // error
def escape2(y: File^): File =
  Lib.mk2(y).value[{}]((x: File) => x) // error
