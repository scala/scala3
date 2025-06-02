package example

class Shadow{

    def shadowParam(x: Int)= {
        val x = true
    }

    def curriedParams(x: Int)(y: List[Int]) = {
        val x = "shadow"
        val y = 1
    }
    

    def multiParams(x: List[Int], y: String) = {
        val x = 1
        val y = List(1,2,3)
    }

    
    def shadowInParamBlock(x: Int, y: Int) = {
        val y = 1
        {
            val x = 2
        }
    }
}