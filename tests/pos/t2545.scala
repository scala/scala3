trait Frog[T] {
      def hello: T
      def size: Int
    }

    trait OnlyWithFrogs {
      self: Frog[?] =>

        def sizeStr = size.toString
    }
