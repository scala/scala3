package base;

public class Caller {
   public void callDoStuff(AbstractBase impl) {
       impl.doStuff("abc");
       impl.doStuff(new Object());
   }
}
