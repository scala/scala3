package dotty.runtime;


/** Unit type representing an erased phantom value.
 *
 *  Based on implementation of BoxedUnit.
 */
public final class ErasedPhantom implements java.io.Serializable {
    private static final long serialVersionUID = 4116021023472525845L;

    public final static ErasedPhantom UNIT = new ErasedPhantom();

    public final static Class<Void> TYPE = java.lang.Void.TYPE;

    private Object readResolve() { return UNIT; }

    private ErasedPhantom() { }

    public boolean equals(java.lang.Object other) {
        return this == other;
    }

    public int hashCode() {
        return 0;
    }

    public String toString() {
        return "(\uD83D\uDC7B )";
    }
}
