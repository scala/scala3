import java.time.OffsetDateTime
import scala.concurrent.duration.*

val dateTime: OffsetDateTime = OffsetDateTime.now()

implicit class DateTimeOps(val dateTime: OffsetDateTime) {
  def plus(amount: FiniteDuration): OffsetDateTime =
    dateTime
}

def test = {
  dateTime.plus(Duration.Zero)
  dateTime.plus(if (true) Duration.Zero else Duration.Zero)
}