public abstract class AbstractChannel {
    protected AbstractChannel() {}
    protected abstract AbstractUnsafe newUnsafe();
    protected abstract class AbstractUnsafe {
        public abstract void connect();
    }
}
