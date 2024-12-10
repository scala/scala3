abstract class InlineConversion[-From, +To]:
  inline def applyInline(inline x: From): To
  extension (x: From) inline def convertInline: To = applyInline(x)

abstract class Conversion[-From, +To] extends InlineConversion[From, To]:
  def apply(x: From): To

  inline def applyInline(inline x: From): To = apply(x)

  extension (x: From) def convert: To = apply(x)

