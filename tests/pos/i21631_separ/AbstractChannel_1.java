public abstract class AbstractChannel_1 {
    protected AbstractChannel_1() {}
    protected abstract AbstractUnsafe newUnsafe();
    protected abstract class AbstractUnsafe {
        public abstract void connect();
    }
}
