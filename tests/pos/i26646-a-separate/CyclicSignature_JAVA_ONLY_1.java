public class CyclicSignature_JAVA_ONLY_1<S extends CyclicSignature_JAVA_ONLY_1<S>.Nested> {
    class Nested {}

    public S test() {
        return null;
    }
}
