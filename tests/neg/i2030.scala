// This used to take ~12s, the check should be that
// it runs in reasonable time (i.e. instantaneous).
object a {
   val x: String | Int = 'a // error
}
