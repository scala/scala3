// Rewrite to indent, keeping 3 spaces as indentation

def m1 = {
   def m2 = {
      "" +
         "" +
         ""
   }
   m2
}

def m4 = {
   def m5 = {
      def m6 = {
         val x = ""
         x
            .apply(0)
            .toString
      }
      m6
         .toString
   }
   m5 +
      m5
         .toString
}
