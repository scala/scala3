package i11401

object Main {

 def main(args:Array[String]):Unit =
     val in = new CIFReader[Boolean](true)
     val select = new SLSelect[Unit]()

     val generator = X.process {
         select.fold(in){ (ch,s) =>
            s.apply1(ch, v=>ch)
         }
     }
     assert(true)

}
