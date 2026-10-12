import scala.annotation.tailrec

case class HandValue(
                      category: Int,
                      categoryName: String,
                      tieBreakers: List[Int]
                    ) extends Ordered[HandValue]:

  override def compare(that: HandValue): Int =
    val categoryComparison =
      category.compare(that.category)

    if categoryComparison != 0 then
      categoryComparison
    else
      HandValue.compareLists(tieBreakers, that.tieBreakers)


object HandValue:

  @tailrec
  private def compareLists(
                            first: List[Int],
                            second: List[Int]
                          ): Int =

    (first, second) match

      case (Nil, Nil) =>
        0

      case (head1 :: _, head2 :: _)
        if head1 != head2 =>
        head1.compare(head2)

      case (_ :: tail1, _ :: tail2) =>
        compareLists(tail1, tail2)

      case (Nil, _ :: _) =>
        -1

      case (_ :: _, Nil) =>
        1


object PokerHandEvaluator:

  /*
   * Category values:
   *
   * 9 = Royal Flush
   * 8 = Straight Flush
   * 7 = Four of a Kind
   * 6 = Full House
   * 5 = Flush
   * 4 = Straight
   * 3 = Three of a Kind
   * 2 = Two Pair
   * 1 = One Pair
   * 0 = High Card
   */

  def evaluate(cards: List[Card]): HandValue =

    require(
      cards.length == 5,
      "A hand must contain exactly 5 cards"
    )

    val ranks =
      cards
        .map(card => rankValue(card.rank))
        .sorted(Ordering.Int.reverse)

    val groups =
      cards
        .groupBy(card => card.rank)
        .values
        .toList
        .sortBy(group =>
          (-group.size, -rankValue(group.head.rank))
        )

    val flush =
      cards.map(_.suit).distinct.size == 1

    val straightHigh =
      findStraightHigh(ranks)

    /*
     * Royal Flush
     */
    if flush && straightHigh.contains(14) then

      HandValue(
        category = 9,
        categoryName = "Royal Flush",
        tieBreakers = List(14)
      )

    /*
     * Straight Flush
     */
    else if flush && straightHigh.nonEmpty then

      HandValue(
        category = 8,
        categoryName = "Straight Flush",
        tieBreakers = List(straightHigh.get)
      )

    /*
     * Four of a Kind
     */
    else if groups.head.size == 4 then

      val fourRank =
        rankValue(groups.head.head.rank)

      val kicker =
        ranks.filter(_ != fourRank).head

      HandValue(
        category = 7,
        categoryName = "Four of a Kind",
        tieBreakers = List(
          fourRank,
          kicker
        )
      )

    /*
     * Full House
     */
    else if
      groups.head.size == 3 &&
        groups(1).size == 2
    then

      HandValue(
        category = 6,
        categoryName = "Full House",
        tieBreakers = List(
          rankValue(groups.head.head.rank),
          rankValue(groups(1).head.rank)
        )
      )

    /*
     * Flush
     */
    else if flush then

      HandValue(
        category = 5,
        categoryName = "Flush",
        tieBreakers = ranks
      )

    /*
     * Straight
     */
    else if straightHigh.nonEmpty then

      HandValue(
        category = 4,
        categoryName = "Straight",
        tieBreakers = List(straightHigh.get)
      )

    /*
     * Three of a Kind
     */
    else if groups.head.size == 3 then

      val tripRank =
        rankValue(groups.head.head.rank)

      val kickers =
        ranks
          .filter(_ != tripRank)
          .take(2)

      HandValue(
        category = 3,
        categoryName = "Three of a Kind",
        tieBreakers =
          tripRank :: kickers
      )

    /*
     * Two Pair
     */
    else if
      groups.head.size == 2 &&
        groups(1).size == 2
    then

      val pairRanks =
        List(
          rankValue(groups.head.head.rank),
          rankValue(groups(1).head.rank)
        ).sorted(Ordering.Int.reverse)

      val kicker =
        ranks
          .filter(rank =>
            !pairRanks.contains(rank)
          )
          .head

      HandValue(
        category = 2,
        categoryName = "Two Pair",
        tieBreakers =
          pairRanks :+ kicker
      )

    /*
     * One Pair
     */
    else if groups.head.size == 2 then

      val pairRank =
        rankValue(groups.head.head.rank)

      val kickers =
        ranks
          .filter(_ != pairRank)
          .take(3)

      HandValue(
        category = 1,
        categoryName = "One Pair",
        tieBreakers =
          pairRank :: kickers
      )

    /*
     * High Card
     */
    else

      HandValue(
        category = 0,
        categoryName = "High Card",
        tieBreakers = ranks
      )


  def bestOfSeven(
                   holeCards: List[Card],
                   communityCards: List[Card]
                 ): HandValue =

    val allCards =
      holeCards ++ communityCards

    require(
      allCards.length >= 5,
      "At least 5 cards are required"
    )

    allCards
      .combinations(5)
      .map(evaluate)
      .max


  private def findStraightHigh(
                                ranks: List[Int]
                              ): Option[Int] =

    val distinctRanks =
      ranks.distinct

    /*
     * Normal straights.
     */
    val normalStraight =
      (14 to 5 by -1)
        .find { high =>
          (high - 4 to high).forall(
            distinctRanks.contains
          )
        }

    normalStraight match

      case Some(high) =>
        Some(high)

      case None =>
        /*
         * Wheel:
         * A-2-3-4-5
         */
        val wheel =
          List(14, 5, 4, 3, 2)

        if wheel.forall(distinctRanks.contains) then
          Some(5)
        else
          None


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