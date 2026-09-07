class CyclicSignature<S extends CyclicSignature<S>.Nested> {
    class Nested {}

    public S test() {
        return null;
    }
}
