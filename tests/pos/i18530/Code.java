package bug.code;

import bug.actions.Smart.Action;

import java.util.ArrayDeque;

public class Code {
    public static class Variable {
        public ArrayDeque<Action> actions = new ArrayDeque<Action>();
    }
}
