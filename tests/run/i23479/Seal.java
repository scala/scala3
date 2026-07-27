// test: -jvm 17+
public sealed interface Seal permits NonSeal {
    default int g() {
        return 42;
    }
}
