package example

class Shadow/*<-example::Shadow#*/{

    def shadowParam/*<-example::Shadow#shadowParam().*/(x/*<-example::Shadow#shadowParam().(x)*/: Int/*->scala::Int#*/)= {
        val x/*<-local0*/ = true
    }

    def curriedParams/*<-example::Shadow#curriedParams().*/(x/*<-example::Shadow#curriedParams().(x)*/: Int/*->scala::Int#*/)(y/*<-example::Shadow#curriedParams().(y)*/: List/*->scala::package.List#*/[Int/*->scala::Int#*/]) = {
        val x/*<-local1*/ = "shadow"
        val y/*<-local2*/ = 1
    }


    def multiParams/*<-example::Shadow#multiParams().*/(x/*<-example::Shadow#multiParams().(x)*/: List/*->scala::package.List#*/[Int/*->scala::Int#*/], y/*<-example::Shadow#multiParams().(y)*/: String/*->scala::Predef.String#*/) = {
        val x/*<-local3*/ = 1
        val y/*<-local4*/ = List/*->scala::package.List.*/(1,2,3)
    }


    def shadowInParamBlock/*<-example::Shadow#shadowInParamBlock().*/(x/*<-example::Shadow#shadowInParamBlock().(x)*/: Int/*->scala::Int#*/, y/*<-example::Shadow#shadowInParamBlock().(y)*/: Int/*->scala::Int#*/) = {
        val y/*<-local5*/ = 1
        {
            val x/*<-local6*/ = 2
        }
    }
}