public class JavaTestServer {
  public static Flow<Message, Message> greeter() {
    return Flow.<Message>create()
        .collect(
            new JavaPartialFunction<Message, Message>() {
              @Override
              public Message apply(Message msg, boolean isCheck) throws Exception {
                if (isCheck)  throw new RuntimeException(); 
                else return handleTextMessage(msg.asTextMessage());
              }
            });
  }

  public static TextMessage handleTextMessage(TextMessage msg) {
    return null;
  }
}

