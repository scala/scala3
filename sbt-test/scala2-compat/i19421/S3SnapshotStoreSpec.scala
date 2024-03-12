import com.typesafe.config.Config
import akka.persistence.snapshot.SnapshotStoreSpec

object ConfigHelper {
  def config(): Config = ???
}

class S3SnapshotStoreSpec extends SnapshotStoreSpec(null)
