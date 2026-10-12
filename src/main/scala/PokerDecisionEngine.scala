
object PokerDecisionEngine {

  final case class ActionEvaluation(
                                     action: PokerAction,
                                     ev: Double,
                                     explanation: String
                                   )

  /**
   * Estimates fold probability from raise size relative to the pot.
   *
   * This is deliberately a simple starting heuristic, not a trained
   * poker model. Tune it using logged hands and observed opponent behavior.
   */
  def estimateFoldProbability(
                               pot: Int,
                               raiseIncrement: Int,
                               opponentAggression: Double = 0.5
                             ): Double = {
    require(pot >= 0, "Pot cannot be negative")
    require(raiseIncrement >= 0, "Raise increment cannot be negative")
    require(
      opponentAggression >= 0.0 && opponentAggression <= 1.0,
      "Opponent aggression must be between 0 and 1"
    )

    if (pot == 0) {
      0.25 * (1.0 - opponentAggression)
    } else {
      val betToPotRatio = raiseIncrement.toDouble / pot

      // Larger bets apply more pressure; aggressive opponents
      // are less likely to fold.
      val baseFoldProbability =
        0.20 + 0.25 * math.min(betToPotRatio, 2.0)

      (baseFoldProbability - 0.15 * opponentAggression)
        .max(0.05)
        .min(0.70)
    }
  }

  /**
   * Chooses the highest-EV legal option from fold/check/call and
   * one candidate raise size.
   *
   * `raiseTo` is a total street bet, not the additional amount.
   * Supply None if a raise isn't legal or you don't want to consider one.
   */
  def chooseAction(
                    pot: Int,
                    playerCurrentBet: Int,
                    highestBet: Int,
                    playerChips: Int,
                    minimumRaise: Int,
                    equity: Double,
                    raiseTo: Option[Int],
                    foldProbability: Double,
                    equityWhenCalled: Option[Double] = None
                  ): ActionEvaluation = {
    require(pot >= 0, "Pot cannot be negative")
    require(playerCurrentBet >= 0, "Current bet cannot be negative")
    require(playerChips >= 0, "Player chips cannot be negative")
    require(minimumRaise > 0, "Minimum raise must be positive")
    require(equity >= 0.0 && equity <= 1.0, "Equity must be between 0 and 1")

    val callCost = math.max(0, highestBet - playerCurrentBet)
    val canCoverCall = callCost <= playerChips
    val canCheck = callCost == 0

    val nonRaiseOptions =
      if (canCheck) {
        List(
          ActionEvaluation(
            PokerAction.Check,
            equity * pot,
            "Check EV assuming no further betting"
          )
        )
      } else {
        val fold = ActionEvaluation(
          PokerAction.Fold,
          0.0,
          "Fold has zero EV relative to this decision"
        )

        if (canCoverCall) {
          List(
            fold,
            ActionEvaluation(
              PokerAction.Call,
              PokerDecisionMath.callEv(pot, callCost, equity),
              "Call EV from estimated equity"
            )
          )
        } else {
          List(fold)
        }
      }


    val raiseOption = raiseTo.flatMap { target =>
      val maximumTotalBet = playerCurrentBet + playerChips
      val isFullRaise = target >= highestBet + minimumRaise
      val fitsStack = target <= maximumTotalBet

      if (isFullRaise && target > highestBet && fitsStack && canCoverCall) {
        val estimatedFoldProbability = foldProbability

        val action =
          if (highestBet == 0) PokerAction.Bet(target)
          else PokerAction.Raise(target)

        Some(
          ActionEvaluation(
            action,
            PokerDecisionMath.raiseEv(
              pot = pot,
              playerCurrentBet = playerCurrentBet,
              highestBet = highestBet,
              raiseTo = target,
              equityWhenCalled = equityWhenCalled.getOrElse(equity),
              foldProbability = estimatedFoldProbability
            ),
            f"Estimated fold probability=$estimatedFoldProbability%.2f"
          )
        )
      } else {
        None
      }
    }

    (nonRaiseOptions ++ raiseOption.toList)
      .maxBy(_.ev)
  }
}