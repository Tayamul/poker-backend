class Deck:

  val cards: List[Card] =
    for
      suit <- Suit.values.toList
      rank <- Rank.values.toList
    yield Card(rank, suit)

  def shuffle(): List[Card] =
    scala.util.Random.shuffle(cards)
    