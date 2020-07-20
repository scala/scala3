import java.util.*;

class WrongBounds {
  class LL<T> extends ArrayList<List<T>> {}
  class Wrap<T extends List<List<?>>> extends ArrayList<T> {}
  class Wrong<T extends LL<?>> extends Wrap<T> {} // error
}
