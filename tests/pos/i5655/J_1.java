public class J_1{
    public class A<T>{
        public class B<U>{}
    }
    A<String>.B<String> m(){
        J_1 j = new J_1();
        A<String> a = j.new A<>();
        A<String>.B<String> b = a.new B<>();
        return b;
    }
}