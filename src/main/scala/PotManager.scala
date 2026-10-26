import scala.collection.mutable

/** Pure contribution-layer construction and deterministic pot settlement. */
object PotManager:
  final case class Contribution(playerId: String, totalContribution: Int, folded: Boolean = false)
  final case class Pot(amount: Int, eligiblePlayerIds: List[String], cap: Int)
  final case class PotBuildResult(pots: List[Pot], uncalledReturns: Map[String, Int]):
    def totalPot: Int = pots.map(_.amount).sum
    def totalReturned: Int = uncalledReturns.values.sum
  final case class Settlement[A](payouts: Map[String, Int], pots: List[Pot], uncalledReturns: Map[String, Int]):
    def totalPaid: Int = payouts.values.sum

  /**
   * A layer at level L contains (L - previousLevel) chips from each player
   * whose total contribution reaches L. Folded players contribute but cannot
   * be eligible to win. A lone contributor's top layer is uncalled and returned.
   */
  def buildPots(players: List[Contribution]): Either[String, PotBuildResult] =
    if players.map(_.playerId).distinct.size != players.size then Left("Player IDs must be unique")
    else if players.exists(_.playerId.trim.isEmpty) then Left("Player IDs cannot be empty")
    else if players.exists(_.totalContribution < 0) then Left("Contributions cannot be negative")
    else
      val levels = players.map(_.totalContribution).filter(_ > 0).distinct.sorted
      val returns = mutable.Map.empty[String, Int].withDefaultValue(0)
      val raw = mutable.ListBuffer.empty[Pot]
      var previous = 0

      levels.foreach { level =>
        val contributors = players.filter(_.totalContribution >= level)
        val increment = level - previous
        val amount = increment * contributors.size
        if contributors.size == 1 then
          val id = contributors.head.playerId
          returns(id) = returns(id) + increment
        else
          raw += Pot(amount, contributors.filterNot(_.folded).map(_.playerId), level)
        previous = level
      }

      // Contributions above every live player's level can be dead money when
      // the contributors folded. Roll those layers into the first eligible pot.
      val deadMoney = raw.filter(_.eligiblePlayerIds.isEmpty).map(_.amount).sum
      val eligiblePots = raw.filter(_.eligiblePlayerIds.nonEmpty).toList
      val withDeadMoney =
        if deadMoney == 0 then Some(eligiblePots)
        else eligiblePots match
          case first :: rest => Some(first.copy(amount = first.amount + deadMoney) :: rest)
          case Nil => None

      withDeadMoney match
        case None => Left("No eligible player can win the contributed pot")
        case Some(pots) => Right(PotBuildResult(pots, returns.toMap))

  /** Higher rank wins. Odd chips follow oddChipOrder, which the caller supplies. */
  def settle[A](
                 players: List[Contribution],
                 handRanks: Map[String, A],
                 oddChipOrder: List[String],
                 ordering: Ordering[A]
               ): Either[String, Settlement[A]] =
    buildPots(players).flatMap { built =>
      val payouts = mutable.Map.empty[String, Int].withDefaultValue(0)
      built.uncalledReturns.foreach { case (id, amount) => payouts(id) += amount }

      var error: Option[String] = None
      built.pots.foreach { pot =>

        val missingRanks =
          pot.eligiblePlayerIds.filterNot(handRanks.contains)

        if missingRanks.nonEmpty then
          error = Some(
            s"Missing hand ranks for eligible players: ${missingRanks.mkString(", ")}"
          )
        else
          val ranked =
            pot.eligiblePlayerIds.map(id => id -> handRanks(id))

          val best =
            ranked.map(_._2).max(ordering)

          val winners =
            ranked.collect {
              case (id, rank) if ordering.compare(rank, best) == 0 => id
            }

          val orderedWinners =
            oddChipOrder.filter(winners.contains) ++
              winners.filterNot(oddChipOrder.contains).sorted

          val share =
            pot.amount / orderedWinners.size

          val odd =
            pot.amount % orderedWinners.size

          orderedWinners.zipWithIndex.foreach { case (id, index) =>
            payouts(id) += share + (if index < odd then 1 else 0)
          }
      }

      error match
        case Some(message) => Left(message)
        case None => Right(Settlement(payouts.toMap, built.pots, built.uncalledReturns))
    }
