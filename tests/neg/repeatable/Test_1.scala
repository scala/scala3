import repeatable._

@Plain_0(1)
@Plain_0(2)
@Plain_0(3)
@FirstLevel_0(Array()) // error
trait U

@FirstLevel_0(Array(Plain_0(4), Plain_0(5)))
@FirstLevel_0(Array(Plain_0(6), Plain_0(7)))
@SecondLevel_0(Array()) // error
trait T

@SecondLevel_0(Array())
@SecondLevel_0(Array()) // error
trait S