
final case class PokerLegalActions(
                                    actions: List[String],
                                    toCall: Int,
                                    minBet: Option[Int],
                                    maxBet: Option[Int],
                                    minRaiseTo: Option[Int],
                                    maxRaiseTo: Option[Int],
                                    betIsAllInOnly: Boolean,
                                    raiseIsShortAllInOnly: Boolean
                                  )

object PokerLegalActions:
  def calculate(
                 game: PokerGame,
                 playerId: String
               ): PokerLegalActions =
    val player = game.players.find(_.id == playerId)

    val finished =
      game.phase == PokerPhase.Showdown ||
        game.phase == PokerPhase.Complete

    val isCurrentPlayer =
      game.currentPlayerId == playerId

    player match
      case None =>
        empty

      case Some(p)
        if finished ||
          !isCurrentPlayer ||
          p.folded ||
          p.allIn =>
        empty

      case Some(p) =>
        val highestBet =
          game.players
            .filterNot(_.folded)
            .map(_.currentBet)
            .max

        val callDifference =
          math.max(0, highestBet - p.currentBet)

        val toCall =
          math.min(callDifference, p.chips)

        val canCheck =
          p.currentBet == highestBet

        val canCall =
          callDifference > 0 && p.chips > 0

        val canBet =
          highestBet == 0 && p.chips > 0

        val minimumBet =
          if p.chips < game.bigBlind then p.chips
          else game.bigBlind

        val canRaise =
          highestBet > p.currentBet &&
            p.chips > 0 &&
            game.raiseAllowedPlayerIds.contains(playerId) &&
            p.currentBet + p.chips > highestBet

        val maximumRaiseTo =
          p.currentBet + p.chips

        val minimumFullRaiseTo =
          highestBet + game.minimumRaise

        val shortAllInOnly =
          canRaise &&
            maximumRaiseTo < minimumFullRaiseTo

        val minimumRaiseTo =
          if !canRaise then None
          else if shortAllInOnly then Some(maximumRaiseTo)
          else Some(minimumFullRaiseTo)

        val actions =
          List("fold") ++
            (if canCheck then List("check") else Nil) ++
            (if canCall then List("call") else Nil) ++
            (if canBet then List("bet") else Nil) ++
            (if canRaise then List("raise") else Nil)

        PokerLegalActions(
          actions = actions,
          toCall = toCall,
          minBet = if canBet then Some(minimumBet) else None,
          maxBet = if canBet then Some(p.chips) else None,
          minRaiseTo = minimumRaiseTo,
          maxRaiseTo = if canRaise then Some(maximumRaiseTo) else None,
          betIsAllInOnly =
            canBet && p.chips < game.bigBlind,
          raiseIsShortAllInOnly = shortAllInOnly
        )

  private def empty: PokerLegalActions =
    PokerLegalActions(
      actions = Nil,
      toCall = 0,
      minBet = None,
      maxBet = None,
      minRaiseTo = None,
      maxRaiseTo = None,
      betIsAllInOnly = false,
      raiseIsShortAllInOnly = false
    )
