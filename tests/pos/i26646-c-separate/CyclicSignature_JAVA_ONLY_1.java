public class CyclicSignature_JAVA_ONLY_1 {
    static class Actual<S extends Actual<S>.Nested.Deeper> {
        class Nested {
            class Deeper {}
        }

        public S test() {
            return null;
        }
    }

}
