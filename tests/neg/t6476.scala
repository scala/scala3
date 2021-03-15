// only the last one doesn't parse
class C {
  s"""\ """
  s"""\\"""
  s"""\"""
  s"\ "
  s"\\"
  s"\" // error
}      // error (should not be one)
