public class Foo{
    String! content;

    public Foo(){
        content = "something";
        super();
    }

    public String! dup(String! s){
        return s + s;
    }

    public void setContent(String! s){
        content = s;
    }
}