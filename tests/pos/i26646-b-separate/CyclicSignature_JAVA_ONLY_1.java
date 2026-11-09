public class CyclicSignature_JAVA_ONLY_1<S extends CyclicSignature_JAVA_ONLY_1<S>.Nested.Deeper> {
    class Nested {
        class Deeper {}
    }

    public S test() {
        return null;
    }
}
