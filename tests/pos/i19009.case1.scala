trait Player[+P]
trait RatingPeriod[P]:
  def games: Map[P, Vector[ScoreVsPlayer[P]]]

trait ScoreVsPlayer[+P]

def updated[P](playerID: P, matchResults: IndexedSeq[ScoreVsPlayer[P]], lookup: P => Option[Player[P]]): Player[P] = ???

trait Leaderboard[P]:
  def  playersByIdInNoParticularOrder: Map[P, Player[P]]

  def after[P2 >: P](ratingPeriod: RatingPeriod[? <: P]): Leaderboard[P2] =
    val competingPlayers = ratingPeriod.games.iterator.map { (id, matchResults) =>
      updated(id, matchResults, playersByIdInNoParticularOrder.get) // error
      // workaround:
      updated[P](id, matchResults, playersByIdInNoParticularOrder.get)
    }
    ???
