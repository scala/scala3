
public sealed interface Seal permits NonSeal {
    default int g() {
        return 42;
    }
}
