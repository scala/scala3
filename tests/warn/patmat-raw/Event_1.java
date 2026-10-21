package repro;

public sealed interface Event_1<T> permits Event_1.Item, Event_1.TimedOut, Event_1.Failed {
  final class Item<T> implements Event_1<T> {}

  @SuppressWarnings("rawtypes")
  final class TimedOut implements Event_1 {}

  @SuppressWarnings("rawtypes")
  final class Failed implements Event_1 {}
}
