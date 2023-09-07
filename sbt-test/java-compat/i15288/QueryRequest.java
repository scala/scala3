interface CopyableBuilder<B, T> {}
interface ToCopyableBuilder<B, T> {}

public class QueryRequest implements ToCopyableBuilder<QueryRequest.Builder, QueryRequest> {
    public static Builder builder() { throw new UnsupportedOperationException(); }
    public interface Builder extends CopyableBuilder<Builder, QueryRequest> {
        void build();
    }
}
