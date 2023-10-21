//> using options -explain
def test1(v: String & Integer & List[String]) = ()
def test2(v: String & Long) = test1(v)  // error

def test3(v: String | Integer | List[String]) = ()
def test4(v: String | Long) = test3(v)  // error
