package repro;

public sealed interface Event<T> permits Event.Item, Event.TimedOut, Event.Failed {
  final class Item<T> implements Event<T> {}

  @SuppressWarnings("rawtypes")
  final class TimedOut implements Event {}

  @SuppressWarnings("rawtypes")
  final class Failed implements Event {}
}
