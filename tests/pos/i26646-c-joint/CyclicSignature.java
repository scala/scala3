public class CyclicSignature {
    static class Actual<S extends Actual<S>.Nested.Deeper> {
        class Nested {
            class Deeper {}
        }

        public S test() {
            return null;
        }
    }

}
