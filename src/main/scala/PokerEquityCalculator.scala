import scala.util.Random

object PokerEquityCalculator:

  def estimateEquity(
                      holeCards: List[Card],
                      communityCards: List[Card],
                      iterations: Int = 5000
                    ): Double =

    require(
      holeCards.length == 2,
      "Exactly 2 hole cards are required"
    )

    require(
      communityCards.length <= 5,
      "Community cards cannot exceed 5"
    )

    require(
      iterations > 0,
      "Iterations must be greater than 0"
    )

    val knownCards =
      holeCards ++ communityCards

    val remainingCards =
      fullDeck
        .filterNot(knownCards.contains)

    var wins = 0
    var ties = 0
    var iteration = 0

    while iteration < iterations do

      val shuffled =
        Random.shuffle(remainingCards)

      val opponentCards =
        shuffled.take(2)

      val cardsAfterOpponent =
        shuffled.drop(2)

      val cardsNeeded =
        5 - communityCards.length

      val simulatedCommunityCards =
        communityCards ++
          cardsAfterOpponent.take(cardsNeeded)

      val aiHand =
        PokerHandEvaluator.bestOfSeven(
          holeCards,
          simulatedCommunityCards
        )

      val opponentHand =
        PokerHandEvaluator.bestOfSeven(
          opponentCards,
          simulatedCommunityCards
        )

      val comparison =
        aiHand.compare(opponentHand)

      if comparison > 0 then
        wins += 1
      else if comparison == 0 then
        ties += 1

      iteration += 1

    (wins + ties * 0.5) /
      iterations.toDouble


  def estimateEquityAgainstRange(
                                  game: PokerGame,
                                  aiPlayerId: String,
                                  iterations: Int = 20000
                                ): Double =

    require(
      iterations > 0,
      "Iterations must be greater than 0"
    )

    val aiPlayer =
      game.players
        .find(_.id == aiPlayerId)
        .getOrElse(
          throw new IllegalArgumentException(
            s"AI player not found: $aiPlayerId"
          )
        )

    require(
      aiPlayer.holeCards.length == 2,
      "AI player must have exactly 2 hole cards"
    )

    require(
      game.communityCards.length <= 5,
      "Community cards cannot exceed 5"
    )

    val range =
      PokerOpponentRange
        .estimateRange(
          game,
          aiPlayerId
        )
        .filter(_.weight > 0)

    val topHands =
      range
        .sortBy(hand => -hand.weight)
        .take(10)

    val totalRangeWeight =
      range.map(_.weight).sum

    println(
      "Top opponent range: " +
        topHands
          .map { hand =>
            val percentage =
              hand.weight / totalRangeWeight * 100.0

            f"${hand.cards.mkString("(", ", ", ")")}=$percentage%.2f%%"
          }
          .mkString(", ")
    )

    if range.isEmpty then
      return 0.0

    val knownCards =
      aiPlayer.holeCards ++
        game.communityCards

    val remainingCards =
      fullDeck
        .filterNot(knownCards.contains)

    var wins = 0
    var ties = 0
    var validIterations = 0

    var iteration = 0

    while iteration < iterations do

      val opponentHand =
        selectWeightedHand(range)

      val cardsAfterOpponent =
        remainingCards
          .filterNot(opponentHand.cards.contains)

      val cardsNeeded =
        5 - game.communityCards.length

      if cardsAfterOpponent.length >= cardsNeeded then

        val simulatedBoard =
          game.communityCards ++
            Random
              .shuffle(cardsAfterOpponent)
              .take(cardsNeeded)

        val aiHand =
          PokerHandEvaluator.bestOfSeven(
            aiPlayer.holeCards,
            simulatedBoard
          )

        val opponentValue =
          PokerHandEvaluator.bestOfSeven(
            opponentHand.cards,
            simulatedBoard
          )

        val comparison =
          aiHand.compare(opponentValue)

        if comparison > 0 then
          wins += 1

        else if comparison == 0 then
          ties += 1

        validIterations += 1

      iteration += 1

    if validIterations == 0 then
      0.0
    else
      (wins + ties * 0.5) /
        validIterations.toDouble


  /**
   * Estimates AI equity against the opponent's range of hands
   * likely to call a proposed raise.
   */
  def estimateEquityAgainstCallingRange(
                                         game: PokerGame,
                                         aiPlayerId: String,
                                         opponentCallCost: Int,
                                         potAfterRaise: Int,
                                         iterations: Int = 5000
                                       ): Double = {
    require(iterations > 0, "Iterations must be greater than 0")
    require(opponentCallCost >= 0, "Call cost cannot be negative")
    require(potAfterRaise >= 0, "Pot cannot be negative")
    require(game.communityCards.length <= 5,
      "Community cards cannot exceed 5")

    val aiPlayer =
      game.players
        .find(_.id == aiPlayerId)
        .getOrElse(
          throw new IllegalArgumentException(
            s"AI player not found: $aiPlayerId"
          )
        )

    require(aiPlayer.holeCards.length == 2,
      "AI player must have exactly 2 hole cards")

    val range =
      PokerOpponentRange
        .estimateCallingRange(
          game,
          aiPlayerId,
          opponentCallCost,
          potAfterRaise
        )
        .filter(_.weight > 0.0)

    if range.isEmpty then
      return 0.0

    val knownCards =
      aiPlayer.holeCards ++ game.communityCards

    val remainingCards =
      fullDeck.filterNot(knownCards.contains)

    var wins = 0
    var ties = 0
    var validIterations = 0
    var iteration = 0

    while iteration < iterations do {
      val opponentHand = selectWeightedHand(range)

      val cardsAfterOpponent =
        remainingCards.filterNot(opponentHand.cards.contains)

      val cardsNeeded = 5 - game.communityCards.length

      if cardsAfterOpponent.length >= cardsNeeded then {
        val simulatedBoard =
          game.communityCards ++
            Random.shuffle(cardsAfterOpponent).take(cardsNeeded)

        val aiValue =
          PokerHandEvaluator.bestOfSeven(
            aiPlayer.holeCards,
            simulatedBoard
          )

        val opponentValue =
          PokerHandEvaluator.bestOfSeven(
            opponentHand.cards,
            simulatedBoard
          )

        val comparison = aiValue.compare(opponentValue)

        if comparison > 0 then wins += 1
        else if comparison == 0 then ties += 1

        validIterations += 1
      }

      iteration += 1
    }

    if validIterations == 0 then 0.0
    else (wins + ties * 0.5) / validIterations.toDouble
  }

  private def selectWeightedHand(
                                  range: List[WeightedHand]
                                ): WeightedHand =

    val totalWeight =
      range.map(_.weight).sum

    val target =
      Random.nextDouble() * totalWeight

    var cumulative = 0.0

    range.find { hand =>
      cumulative += hand.weight
      cumulative >= target
    }.getOrElse(range.last)


  private def fullDeck: List[Card] =
    for
      suit <- Suit.values.toList
      rank <- Rank.values.toList
    yield Card(rank, suit)