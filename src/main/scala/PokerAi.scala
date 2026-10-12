object PokerAi:

  def decide(
              game: PokerGame,
              playerId: String
            ): PokerAction =

    val player =
      game.players
        .find(_.id == playerId)
        .getOrElse(
          throw new IllegalArgumentException(
            s"Player not found: $playerId"
          )
        )

    val activePlayers =
      game.players.filter(!_.folded)

    val highestBet =
      activePlayers
        .map(_.currentBet)
        .max

    val amountToCall =
      math.max(
        0,
        highestBet - player.currentBet
      )

    /*
     * Estimate our chance of winning against a random
     * opponent hand.
     *
     * We use fewer simulations than our standalone tests
     * because the AI may make several decisions during
     * one hand.
     */
    val equity =
      PokerEquityCalculator.estimateEquityAgainstRange(
        game,
        playerId,
        iterations =
          if game.phase == PokerPhase.PreFlop then 3000
          else 2500
      )
    println("\n------------------------------ \n")
    println(
      s"AI range equity = $equity"
    )
    println("\n")
    println(
      s"AI action history = ${game.actionHistory}"
    )
    println("\n")
    println(
      s"AI last action = ${game.lastAction}"
    )
//    println("\n------------------------------ \n")

    if game.phase == PokerPhase.PreFlop then

      decidePreFlop(
        game,
        player,
        amountToCall,
        equity
      )

    else

      val hand =
        PokerHandEvaluator.bestOfSeven(
          player.holeCards,
          game.communityCards
        )

      val hasFlushDraw =
        isFlushDraw(
          player.holeCards ++ game.communityCards
        )

      val hasStraightDraw =
        isStraightDraw(
          player.holeCards ++ game.communityCards
        )

      decidePostFlop(
        game,
        player,
        amountToCall,
        hand.category,
        hasFlushDraw,
        hasStraightDraw,
        equity
      )


  private def decidePreFlop(
                             game: PokerGame,
                             player: PokerPlayer,
                             amountToCall: Int,
                             equity: Double
                           ): PokerAction =
    decideWithEngine(game, player, equity)


  private def decidePostFlop(
                              game: PokerGame,
                              player: PokerPlayer,
                              amountToCall: Int,
                              category: Int,
                              hasFlushDraw: Boolean,
                              hasStraightDraw: Boolean,
                              equity: Double
                            ): PokerAction =
    decideWithEngine(game, player, equity)


  private def calculatePotOdds(
                                pot: Int,
                                amountToCall: Int
                              ): Double =

    if amountToCall <= 0 then
      0.0
    else
      amountToCall.toDouble /
        (pot + amountToCall).toDouble


  private def isFlushDraw(
                           cards: List[Card]
                         ): Boolean =

    cards
      .groupBy(_.suit)
      .values
      .exists(_.size == 4)


  private def isStraightDraw(
                              cards: List[Card]
                            ): Boolean =

    val ranks =
      cards
        .map(card => rankValue(card.rank))
        .distinct

    val normalDraw =
      (14 to 6 by -1).exists { high =>

        val straightRanks =
          (high - 4 to high).toSet

        straightRanks
          .intersect(ranks.toSet)
          .size >= 4
      }

    val wheelRanks =
      Set(14, 2, 3, 4, 5)

    val wheelDraw =
      wheelRanks
        .intersect(ranks.toSet)
        .size >= 4

    normalDraw || wheelDraw


  private def raiseIfPossible(
                               game: PokerGame,
                               player: PokerPlayer,
                               amountToCall: Int,
                               multiplier: Int
                             ): PokerAction =

    val activePlayers =
      game.players.filter(!_.folded)

    val highestBet =
      activePlayers
        .map(_.currentBet)
        .max

    /*
     * minimumRaise is the size of the previous raise.
     *
     * Therefore the minimum legal total bet is:
     *
     * highest bet + minimum raise
     */
    val minimumRaiseAmount =
      highestBet + game.minimumRaise

    val desiredRaise =
      highestBet +
        game.bigBlind * multiplier

    val raiseAmount =
      math.max(
        minimumRaiseAmount,
        desiredRaise
      )

    val maximumBet =
      player.currentBet + player.chips

    if maximumBet <= highestBet then

      PokerAction.Call

    else if maximumBet < raiseAmount then

      PokerAction.Call

    else

      PokerAction.Raise(
        raiseAmount
      )


  private def rankValue(
                         rank: Rank
                       ): Int =

    rank match

      case Rank.Two   => 2
      case Rank.Three => 3
      case Rank.Four  => 4
      case Rank.Five  => 5
      case Rank.Six   => 6
      case Rank.Seven => 7
      case Rank.Eight => 8
      case Rank.Nine  => 9
      case Rank.Ten   => 10
      case Rank.Jack  => 11
      case Rank.Queen => 12
      case Rank.King  => 13
      case Rank.Ace   => 14


  private def decideWithEngine(
                                game: PokerGame,
                                player: PokerPlayer,
                                equity: Double
                              ): PokerAction = {
    val activePlayers =
      game.players.filterNot(_.folded)

    val highestBet =
      activePlayers.map(_.currentBet).maxOption.getOrElse(0)

    val opponent =
      activePlayers.find(_.id != player.id)

    val minimumRaiseTo =
      highestBet + game.minimumRaise

    // Use a modest pot-aware candidate rather than a very large raise.
    val desiredRaiseTo =
      if highestBet == 0 then
        math.max(game.bigBlind * 2, game.minimumRaise)
      else
        highestBet + math.max(game.minimumRaise, game.pot / 2)

    val playerMaximumBet =
      player.currentBet + player.chips

    val opponentMaximumBet =
      opponent.map(p => p.currentBet + p.chips).getOrElse(0)

    // The initial model only evaluates raises the opponent can fully cover.
    val maximumCoveredRaiseTo =
      math.min(playerMaximumBet, opponentMaximumBet)

    val proposedRaiseTo =
      math.min(
        math.max(desiredRaiseTo, minimumRaiseTo),
        maximumCoveredRaiseTo
      )

    val hasLegalRaise =
      opponent.nonEmpty &&
        proposedRaiseTo >= minimumRaiseTo &&
        proposedRaiseTo > highestBet

    val equityWhenCalled =
      if hasLegalRaise then {
        opponent.flatMap { other =>
          val opponentCallCost =
            proposedRaiseTo - other.currentBet

          val opponentCanCover =
            opponentCallCost >= 0 &&
              opponentCallCost <= other.chips

          if opponentCanCover then {
            val potAfterRaise =
              game.pot +
                (proposedRaiseTo - player.currentBet)

            Some(
              PokerEquityCalculator.estimateEquityAgainstCallingRange(
                game = game,
                aiPlayerId = player.id,
                opponentCallCost = opponentCallCost,
                potAfterRaise = potAfterRaise,
                iterations = 4000
              )
            )
          } else {
            None
          }
        }
      } else {
        None
      }

    val foldProbability =
      if hasLegalRaise then
        PokerDecisionEngine.estimateFoldProbability(
          pot = game.pot,
          raiseIncrement = proposedRaiseTo - highestBet,
          opponentAggression = 0.5
        )
      else
        0.0

    val result =
      PokerDecisionEngine.chooseAction(
        pot = game.pot,
        playerCurrentBet = player.currentBet,
        highestBet = highestBet,
        playerChips = player.chips,
        minimumRaise = game.minimumRaise,
        equity = equity,
        raiseTo =
          if hasLegalRaise then Some(proposedRaiseTo)
          else None,
        foldProbability = foldProbability,
        equityWhenCalled = equityWhenCalled
      )

    println(f"Equity against overall range: $equity%.3f")

    equityWhenCalled.foreach { value =>
      println(f"Equity against calling range: $value%.3f")
    }

    println(
      f"Decision engine: ${result.action}, EV=${result.ev}%.2f, " +
        s"reason=${result.explanation}"
    )

    result.action
  }
