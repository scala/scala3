class CyclicSignature<S extends CyclicSignature<S>.Nested.Deeper> {
    class Nested {
        class Deeper {}
    }

    public S test() {
        return null;
    }
}
