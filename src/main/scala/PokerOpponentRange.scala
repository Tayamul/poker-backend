case class WeightedHand(
                         cards: List[Card],
                         weight: Double
                       )

object PokerOpponentRange:

  /**
   * Builds a weighted range for the opponent based on:
   *
   * - Known cards
   * - Pre-flop actions
   * - Post-flop aggression
   * - Bet sizing
   *
   * Weight is relative likelihood, not a percentage.
   */
  def estimateRange(
                     game: PokerGame,
                     aiPlayerId: String
                   ): List[WeightedHand] =

    val aiPlayer =
      game.players
        .find(_.id == aiPlayerId)
        .getOrElse(
          throw new IllegalArgumentException(
            s"AI player not found: $aiPlayerId"
          )
        )

    val opponent =
      game.players
        .find(_.id != aiPlayerId)
        .getOrElse(
          throw new IllegalArgumentException(
            "Opponent not found"
          )
        )

    val knownCards =
      aiPlayer.holeCards ++
        game.communityCards

    val possibleCards =
      fullDeck.filterNot(knownCards.contains)

    val opponentActions =
      game.actionHistory.filter(
        _.playerId == opponent.id
      )

    val baseRange =
      possibleCards
        .combinations(2)
        .map { cards =>
          WeightedHand(
            cards = cards,
            weight = startingWeight(cards)
          )
        }
        .toList

    baseRange.map { hand =>
      hand.copy(
        weight =
          adjustWeight(
            hand.cards,
            hand.weight,
            opponentActions,
            game
          )
      )
    }


  /**
   * Estimates the opponent's range conditional on calling a proposed bet.
   *
   * This is a heuristic filter over the existing weighted range.
   * It is not a statistically trained calling model.
   */
  def estimateCallingRange(
                            game: PokerGame,
                            aiPlayerId: String,
                            opponentCallCost: Int,
                            potAfterRaise: Int
                          ): List[WeightedHand] = {
    require(opponentCallCost >= 0, "Call cost cannot be negative")
    require(potAfterRaise >= 0, "Pot cannot be negative")

    val initialRange = estimateRange(game, aiPlayerId)

    val potOdds =
      if opponentCallCost == 0 then 0.0
      else opponentCallCost.toDouble /
        (potAfterRaise + opponentCallCost).toDouble

    initialRange.flatMap { hand =>
      val likelihood =
        callingLikelihood(
          hand.cards,
          game.communityCards,
          game.phase,
          potOdds
        )

      val adjustedWeight = hand.weight * likelihood

      if adjustedWeight > 0.0 then
        Some(hand.copy(weight = adjustedWeight))
      else
        None
    }
  }

  /**
   * Relative likelihood that a candidate hand continues against a bet.
   * Strong made hands and draws retain more of their original weight.
   */
  private def callingLikelihood(
                                 holeCards: List[Card],
                                 board: List[Card],
                                 phase: PokerPhase,
                                 potOdds: Double
                               ): Double = {
    val strength =
      if phase == PokerPhase.PreFlop || board.length < 3 then
        preFlopContinueStrength(holeCards)
      else
        val category =
          PokerHandEvaluator.bestOfSeven(holeCards, board).category

        category match {
          case 9 => 1.00 // Royal flush
          case 8 => 1.00 // Straight flush
          case 7 => 1.00 // Four of a kind
          case 6 => 0.99 // Full house
          case 5 => 0.97 // Flush
          case 4 => 0.95 // Straight
          case 3 => 0.92 // Three of a kind
          case 2 => 0.85 // Two pair
          case 1 => 0.65 // One pair
          case _ =>
            if hasStrongDraw(holeCards, board) then 0.48
            else 0.16
        }

    // Larger pot odds put more pressure on marginal hands.
    // Strong hands are less affected than weak hands.
    val pressureSensitivity = 1.0 - strength
    val pressureAdjustment =
      1.0 - (potOdds * pressureSensitivity)

    (strength * pressureAdjustment).max(0.02).min(1.0)
  }

  private def preFlopContinueStrength(
                                       cards: List[Card]
                                     ): Double = {
    val first = rankValue(cards.head.rank)
    val second = rankValue(cards(1).rank)
    val high = math.max(first, second)
    val low = math.min(first, second)
    val suited = cards.head.suit == cards(1).suit
    val pair = first == second
    val gap = high - low

    if pair && high >= 11 then 0.98
    else if high == 14 && low >= 12 then 0.92
    else if pair then 0.78
    else if high == 14 && low >= 10 then 0.75
    else if suited && high >= 11 then 0.62
    else if high >= 11 && low >= 9 then 0.52
    else if suited && gap <= 2 && high >= 8 then 0.40
    else 0.20
  }


  private def startingWeight(
                              cards: List[Card]
                            ): Double =

    val first = rankValue(cards.head.rank)
    val second = rankValue(cards(1).rank)

    val high = math.max(first, second)
    val low = math.min(first, second)

    val pair =
      first == second

    val suited =
      cards.head.suit == cards(1).suit

    val gap =
      high - low

    /*
     * Give stronger starting hands higher
     * base weights.
     */
    if pair && high >= 12 then
      10.0
    else if pair && high >= 9 then
      7.0
    else if pair then
      4.0
    else if high == 14 && low >= 10 then
      8.0
    else if high == 14 then
      5.0
    else if suited && high >= 11 then
      5.0
    else if suited && gap <= 2 then
      3.5
    else if gap <= 1 && high >= 8 then
      3.0
    else
      1.0


  private def adjustWeight(
                            cards: List[Card],
                            baseWeight: Double,
                            actions: List[PokerActionRecord],
                            game: PokerGame
                          ): Double =

    var weight = baseWeight

    actions.foreach { record =>

      val board =
        record.communityCards

      val handStrength =
        estimateMadeHandStrength(
          cards,
          board
        )

      val hasPair =
        cards.head.rank == cards(1).rank

      record.action match

        case PokerAction.Raise(amount) =>

          if record.phase == PokerPhase.PreFlop then

            if isPremiumPreFlop(cards) then
              weight *= 1.8

            else if isStrongPreFlop(cards) then
              weight *= 1.3

            else
              weight *= 0.55

          else if handStrength >= 3 then
            weight *= 1.7

          else if hasStrongDraw(cards, board) then
            weight *= 1.25

          else
            weight *= 0.55


        case PokerAction.Bet(amount) =>

          if record.phase == PokerPhase.PreFlop then

            if isPremiumPreFlop(cards) then
              weight *= 1.4

            else if isStrongPreFlop(cards) then
              weight *= 1.15

            else
              weight *= 0.8

          else if handStrength >= 3 then
            weight *= 1.45

          else if handStrength >= 1 then
            weight *= 1.15

          else if hasStrongDraw(cards, board) then
            weight *= 1.10

          else
            weight *= 0.8

          val pot =
            math.max(record.potBefore, 1)

          val betRatio =
            amount.toDouble / pot.toDouble

          if betRatio >= 0.75 then
            weight *= 1.15

          else if betRatio >= 0.40 then
            weight *= 1.05


        case PokerAction.Call =>

          if record.phase == PokerPhase.PreFlop then

            if isPremiumPreFlop(cards) then
              weight *= 0.85

            else if isStrongPreFlop(cards) then
              weight *= 1.10

            else
              weight *= 1.0

          else if handStrength >= 1 then
            weight *= 1.15

          else if hasStrongDraw(cards, board) then
            weight *= 1.10

          else
            weight *= 1.0


        case PokerAction.Check =>

          if handStrength >= 5 then
            weight *= 0.75

          else if handStrength >= 2 then
            weight *= 0.95

          else
            weight *= 1.05


        case PokerAction.Fold =>

          weight = 0.0
    }

    if weight <= 0.0 then 0.0 else math.max(weight, 0.0001)


  private def isPremiumPreFlop(
                                cards: List[Card]
                              ): Boolean =

    val first = rankValue(cards.head.rank)
    val second = rankValue(cards(1).rank)

    val high = math.max(first, second)
    val low = math.min(first, second)

    (first == second && high >= 11) ||
      (high == 14 && low >= 13)


  private def isStrongPreFlop(
                               cards: List[Card]
                             ): Boolean =

    val first = rankValue(cards.head.rank)
    val second = rankValue(cards(1).rank)

    val high = math.max(first, second)
    val low = math.min(first, second)

    val suited =
      cards.head.suit == cards(1).suit

    val pair =
      first == second

    pair ||
      (high == 14 && low >= 10) ||
      (suited && high >= 11) ||
      (high >= 11 && low >= 9)


  private def estimateMadeHandStrength(
                                        holeCards: List[Card],
                                        communityCards: List[Card]
                                      ): Int =

    if communityCards.length < 3 then
      0
    else
      PokerHandEvaluator
        .bestOfSeven(
          holeCards,
          communityCards
        )
        .category


  private def hasStrongDraw(
                             holeCards: List[Card],
                             communityCards: List[Card]
                           ): Boolean =

    val cards =
      holeCards ++ communityCards

    val flushDraw =
      cards
        .groupBy(_.suit)
        .values
        .exists(_.size >= 4)

    val ranks =
      cards
        .map(card => rankValue(card.rank))
        .distinct
        .toSet

    val straightDraw =
      (5 to 14).exists { high =>
        val required =
          (high - 4 to high).toSet

        required.intersect(ranks).size >= 4
      }

    flushDraw || straightDraw


  private def fullDeck: List[Card] =
    for
      suit <- Suit.values.toList
      rank <- Rank.values.toList
    yield Card(rank, suit)


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