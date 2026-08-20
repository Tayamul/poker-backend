case class Card(rank: Rank, suit: Suit):
  override def toString: String =
    val rankSymbol = rank match
      case Rank.Ace   => "A"
      case Rank.King  => "K"
      case Rank.Queen => "Q"
      case Rank.Jack  => "J"
      case Rank.Ten   => "10"
      case Rank.Nine  => "9"
      case Rank.Eight => "8"
      case Rank.Seven => "7"
      case Rank.Six   => "6"
      case Rank.Five  => "5"
      case Rank.Four  => "4"
      case Rank.Three => "3"
      case Rank.Two   => "2"

    val suitSymbol = suit match
      case Suit.Diamonds => "♦"
      case Suit.Spades   => "♠"
      case Suit.Clubs    => "♣"
      case Suit.Hearts   => "♥"

    s"$rankSymbol$suitSymbol"