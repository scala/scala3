import scala.language.experimental.namedTuples

type City = (name: String)

def getCityInfo(city: Option[City]) = city match
  case Some(iam = n) => // error
    n // error
  case _ =>

def getCiryInfo1(city: Option[(name: String, population: Int)]) = city match
  case Some(iam = n) => // error
    n // error
  case _ =>