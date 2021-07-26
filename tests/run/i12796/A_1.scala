object A:
  type Timeframe = "1m" | "2m" | "1H"

  def test(input: String) = input.isInstanceOf[Timeframe]
