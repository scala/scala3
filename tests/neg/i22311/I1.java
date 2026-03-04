public interface I1 extends I0<String> {
    @Override
    default String func(String s) {
        return s;
    }
}
