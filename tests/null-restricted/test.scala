class A(val s : String):
    val t : String = "hello"
    var w: String = "setterNeeded"

    def foo(t: String): String = {
        val x = " right"
        var y = " left"
        t ++ x ++ y
    }

    def dupString(t: String): (String, String) = {
        val x = " right"
        var y = " left"
        (t ++ x, t ++ y)
    }
    
    def currying(t: String)(u: String): (String, String) = {
        val x = " right"
        var y = " left"
        (t ++ x, u ++ y)
    }
    // val s : String | Null = null

    // def foo(t: String | Null): String | Null = {
    //     if (t != null && s != null && t.size > s.size){
    //         t
    //     }
    //     else{
    //         s
    //     }
    // }

class Main:
    def main(args: Array[String]): Unit = {
        val a = new A("sam")
    }