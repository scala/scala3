    class Base {
      class Ops {  }
    }

    class Sub extends Base {
      class Ops {  } // error: cannot override
    }
