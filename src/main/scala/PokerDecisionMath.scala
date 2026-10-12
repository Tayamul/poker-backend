
object PokerDecisionMath:

  /**
   * Estimated net EV of calling, measured from the
   * moment the decision is made.
   *
   * potBeforeCall already includes all chips currently
   * committed to the pot, including the opponent's bet.
   *
   * Assumes no further betting and a heads-up showdown.
   */

  /**
   * Expected value of calling, measured from the current decision.
   *
   * Assumes the call is fully matched and ignores future betting.
   * `pot` is the pot before this player acts and does not include
   * the chips they would add by calling.
   */
  def callEv(
              potBeforeCall: Int,
              callCost: Int,
              equity: Double
            ): Double =
    require(potBeforeCall >= 0, "Pot cannot be negative")
    require(callCost >= 0, "Call cost cannot be negative")
    require(equity >= 0.0 && equity <= 1.0, "Equity must be between 0 and 1")

    if callCost == 0 then
      0.0
    else
      equity * (potBeforeCall + callCost) - callCost


  /**
   * EV of raising to `raiseTo`, where raiseTo is the player's
   * total contribution on this betting street after raising.
   *
   * `foldProbability` is the estimated probability all opponents
   * fold to the raise. This initial implementation is heads-up.
   *
   * Assumes the opponent can cover the raise and excludes side pots,
   * future betting, rake and tournament ICM.
   */
  
  def raiseEv(
               pot: Int,
               playerCurrentBet: Int,
               highestBet: Int,
               raiseTo: Int,
               equityWhenCalled: Double,
               foldProbability: Double
             ): Double = {
    require(pot >= 0, "Pot cannot be negative")
    require(playerCurrentBet >= 0, "Current bet cannot be negative")
    require(
      highestBet >= playerCurrentBet,
      "Highest bet cannot be below player's bet"
    )
    require(raiseTo > highestBet, "Raise-to amount must exceed highest bet")
    require(
      equityWhenCalled >= 0.0 && equityWhenCalled <= 1.0,
      "Equity must be between 0 and 1"
    )
    require(
      foldProbability >= 0.0 && foldProbability <= 1.0,
      "Fold probability must be between 0 and 1"
    )

    val playerInvestment = raiseTo - playerCurrentBet
    val opponentAdditionalCall = raiseTo - highestBet

    // If the opponent folds, we win the existing pot.
    val evIfOpponentFolds = pot.toDouble

    // If called, both players' new chips enter the pot.
    val finalPotIfCalled =
      pot + playerInvestment + opponentAdditionalCall

    val evIfCalled =
      equityWhenCalled * finalPotIfCalled - playerInvestment

    foldProbability * evIfOpponentFolds +
      (1.0 - foldProbability) * evIfCalled
  }


  /**
   * Equity required to break even on a call under
   * the same no-future-betting assumption.
   */
  def breakEvenEquity(
                       potBeforeCall: Int,
                       callCost: Int
                     ): Double =
    require(potBeforeCall >= 0, "Pot cannot be negative")
    require(callCost >= 0, "Call cost cannot be negative")

    if callCost == 0 then
      0.0
    else
      callCost.toDouble / (potBeforeCall + callCost)