// A generic Java class (`Schema`) and an `ObjectMapper`-like helper whose
// `readValue` mirrors Jackson's `<T> T readValue(Class<T>)`.
public class J {
    public static class Schema<T> {}

    public static class Mapper {
        public Schema<?> createProperty() { return null; }
        public <T> T readValue(Class<T> cls) { return null; }
    }
}
