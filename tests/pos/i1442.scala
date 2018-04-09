object Test1442 {
  final def sumMinimized[B](num: Numeric[B]): Int = {
       var cse: scala.math.Numeric.type = null.asInstanceOf[scala.math.Numeric.type]
       ({cse = scala.math.Numeric; num eq cse.IntIsIntegral} ||
               (num eq cse.BigDecimalAsIfIntegral))
      2
  } 

  final def sum[B](implicit num: Numeric[B]): B = {
           // arithmetic series formula  can be used for regular addition
       var cse: scala.math.Numeric.type = null.asInstanceOf[scala.math.Numeric.type]   
       if ({cse = scala.math.Numeric; num eq cse.IntIsIntegral}||
               (num eq cse.BigIntIsIntegral)||
               (num eq cse.ShortIsIntegral)||
               (num eq cse.ByteIsIntegral)||
               (num eq cse.CharIsIntegral)||
               (num eq cse.LongIsIntegral)||
               (num eq cse.BigDecimalIsFractional)) {     
        null.asInstanceOf[B]
      } else null.asInstanceOf[B]
  } 
}
