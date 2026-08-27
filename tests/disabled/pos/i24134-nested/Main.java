public class Main {
    public static void main(String[] args) {
        Container<String> container = new Container<>();
        Container<String>.JavaPartialFunction<Integer> pf = container.new JavaPartialFunction<Integer>() {
            @Override
            public boolean isDefinedAt(String x) {
                return x != null && !x.isEmpty();
            }
            @Override
            public Integer apply(String x) {
                return x.length();
            }
        };
    }
}
