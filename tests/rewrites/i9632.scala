def f(using scala.quoted.QuoteContext) =
    '{
        val x = ${
          ???
        }
        x
     }
