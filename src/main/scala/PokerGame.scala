enum PokerPhase:
  case PreFlop
  case Flop
  case Turn
  case River
  case Showdown
  case Complete


enum PokerAction:
  case Fold
  case Check
  case Call
  case Bet(amount: Int)
  case Raise(amount: Int)


case class PokerPlayer(
                        id: String,
                        name: String,
                        chips: Int,
                        holeCards: List[Card],
                        currentBet: Int,
                        folded: Boolean,
                        allIn: Boolean,
                        totalContribution: Int = 0
                      )


case class PokerActionRecord(
                              playerId: String,
                              phase: PokerPhase,
                              action: PokerAction,
                              communityCards: List[Card],
                              potBefore: Int
                            )


case class PokerGame(
                      players: List[PokerPlayer],
                      deck: List[Card],
                      communityCards: List[Card],
                      pot: Int,
                      dealerPlayerId: String,
                      smallBlind: Int,
                      bigBlind: Int,
                      phase: PokerPhase,
                      currentPlayerId: String,
                      minimumRaise: Int,
                      winnerPlayerId: Option[String] = None,
                      winnerPlayerIds: List[String] = Nil,
                      winningHand: Option[String] = None,
                      lastAction: Option[String] = None,
                      actedPlayerIds: Set[String] = Set.empty,
                      actionHistory: List[PokerActionRecord] = Nil
                    )


object PokerGame:

  def start(
             startingChips: Int = 1000,
             smallBlind: Int = 5,
             bigBlind: Int = 10
           ): PokerGame =

    val shuffledDeck = new Deck().shuffle()

    val player1Cards = List(
      shuffledDeck(0),
      shuffledDeck(2)
    )

    val player2Cards = List(
      shuffledDeck(1),
      shuffledDeck(3)
    )

    val remainingDeck = shuffledDeck.drop(4)

    val player1 = PokerPlayer(
      id = "player1",
      name = "You",
      chips = startingChips - smallBlind,
      holeCards = player1Cards,
      currentBet = smallBlind,
      folded = false,
      allIn = false
    )

    val player2 = PokerPlayer(
      id = "player2",
      name = "Opponent",
      chips = startingChips - bigBlind,
      holeCards = player2Cards,
      currentBet = bigBlind,
      folded = false,
      allIn = false
    )

    println("\n--------------- Player 1 ---------------")
    println(player1)
    println("\n--------------- Player 2 ---------------")
    println(player2)
//    println("\n--------------- Player 2's hole cards ---------------")
//    println(player2.holeCards)
//    println("\n------------------------------ \n")

    PokerGame(
      players = List(player1, player2),
      deck = remainingDeck,
      communityCards = Nil,
      pot = smallBlind + bigBlind,
      dealerPlayerId = "player1",
      smallBlind = smallBlind,
      bigBlind = bigBlind,
      phase = PokerPhase.PreFlop,
      currentPlayerId = "player1",
      minimumRaise = bigBlind
    )


  def applyAction(
                   game: PokerGame,
                   action: PokerAction
                 ): Either[String, PokerGame] =

//    println("\n--------------- Game = ")

//    println("\n--------------- applyAction game.actionHistory ---------------")
//    println(game.actionHistory)
//    println("\n------------------------------ \n")

    val potBefore = game.pot
    println("\n Pot before = " + potBefore)
    println("\n Game = " + game)

    if game.phase == PokerPhase.Showdown ||
      game.phase == PokerPhase.Complete then
      return Left("The hand is already finished")

    val player =
      game.players.find(_.id == game.currentPlayerId)

    if player.isEmpty then
      return Left("No current player")

    val currentPlayer = player.get

    if currentPlayer.folded then
      return Left("This player has folded")

    if currentPlayer.allIn then
      return Left("This player is all-in")

    val highestBet =
      game.players
        .filter(!_.folded)
        .map(_.currentBet)
        .max

    action match

      case PokerAction.Fold =>
        fold(
          recordAction(
            game,
            currentPlayer,
            action,
            potBefore
          ),
          currentPlayer
        )

      case PokerAction.Check =>
        if currentPlayer.currentBet != highestBet then
          Left("You cannot check because you must call the current bet")
        else
          val updatedGame =
            recordAction(
              game.copy(
                lastAction =
                  Some(s"${currentPlayer.name} checked")
              ),
              currentPlayer,
              action,
              potBefore
            )

          finishAction(updatedGame)

      case PokerAction.Call =>
        if highestBet <= currentPlayer.currentBet then
          Left("There is nothing to call")
        else
          val amountToCall =
            highestBet - currentPlayer.currentBet

          val actualAmount =
            math.min(amountToCall, currentPlayer.chips)

          val updatedPlayer =
            currentPlayer.copy(
              chips = currentPlayer.chips - actualAmount,
              currentBet = currentPlayer.currentBet + actualAmount,
              allIn = currentPlayer.chips - actualAmount == 0
            )

          var updatedPlayers =
            game.players.map { p =>
              if p.id == currentPlayer.id then updatedPlayer
              else p
            }

          var updatedPot =
            game.pot + actualAmount

          /*
           * Heads-up special case:
           *
           * If the player cannot fully call because they
           * are all-in, any unmatched portion of the
           * opponent's bet is returned.
           */
          if actualAmount < amountToCall then

            val opponent =
              updatedPlayers.find(_.id != currentPlayer.id).get

            val unmatchedAmount =
              amountToCall - actualAmount

            val refundedOpponent =
              opponent.copy(
                chips = opponent.chips + unmatchedAmount,
                currentBet = opponent.currentBet - unmatchedAmount
              )

            updatedPlayers =
              updatedPlayers.map { p =>
                if p.id == opponent.id then refundedOpponent
                else p
              }

            updatedPot =
              updatedPot - unmatchedAmount

          val updatedGame =
            recordAction(
              game.copy(
                players = updatedPlayers,
                pot = updatedPot,
                lastAction =
                  Some(s"${currentPlayer.name} called $actualAmount")
              ),
              currentPlayer,
              action,
              potBefore
            )

          finishAction(updatedGame)

      case PokerAction.Bet(amount) =>
        if highestBet != 0 then
          Left("A bet has already been made. You must call or raise.")
        else if amount < game.bigBlind then
          Left(s"The minimum bet is ${game.bigBlind}")
        else if amount > currentPlayer.chips then
          Left("You do not have enough chips for that bet")
        else
          val updatedPlayer =
            currentPlayer.copy(
              chips = currentPlayer.chips - amount,
              currentBet = amount,
              allIn = currentPlayer.chips - amount == 0
            )

          val updatedGame =
            recordAction(
              game.copy(
                players =
                  game.players.map { p =>
                    if p.id == currentPlayer.id then updatedPlayer
                    else p
                  },
                pot = game.pot + amount,
                minimumRaise = amount,
                actedPlayerIds = Set(currentPlayer.id),
                lastAction =
                  Some(s"${currentPlayer.name} bet $amount")
              ),
              currentPlayer,
              action,
              potBefore
            )

          finishAction(updatedGame)

      case PokerAction.Raise(amount) =>
        if highestBet <= currentPlayer.currentBet then
          Left("There is no bet to raise")
        else
          val minimumTotal =
            highestBet + game.minimumRaise

          if amount < minimumTotal then
            Left(
              s"Minimum raise is to $minimumTotal"
            )
          else if amount <= currentPlayer.currentBet then
            Left("Raise amount must be greater than your current bet")
          else
            val chipsRequired =
              amount - currentPlayer.currentBet

            if chipsRequired > currentPlayer.chips then
              Left("You do not have enough chips for that raise")
            else
              val raiseSize =
                amount - highestBet

              val updatedPlayer =
                currentPlayer.copy(
                  chips = currentPlayer.chips - chipsRequired,
                  currentBet = amount,
                  allIn =
                    currentPlayer.chips - chipsRequired == 0
                )

              val updatedGame =
                recordAction(
                  game.copy(
                    players =
                      game.players.map { p =>
                        if p.id == currentPlayer.id then updatedPlayer
                        else p
                      },
                    pot = game.pot + chipsRequired,
                    minimumRaise = raiseSize,
                    actedPlayerIds = Set(currentPlayer.id),
                    lastAction =
                      Some(
                        s"${currentPlayer.name} raised to $amount"
                      )
                  ),
                  currentPlayer,
                  action,
                  potBefore
                )

              finishAction(updatedGame)


  private def fold(
                    game: PokerGame,
                    player: PokerPlayer
                  ): Either[String, PokerGame] =

    val foldedPlayers =
      game.players.map { p =>
        if p.id == player.id then
          p.copy(folded = true)
        else
          p
      }

    val winner =
      foldedPlayers.find(!_.folded)

    winner match
      case None =>
        Left("No player remains")

      case Some(winnerPlayer) =>

        val paidWinner =
          winnerPlayer.copy(
            chips = winnerPlayer.chips + game.pot
          )

        val finalPlayers =
          foldedPlayers.map { p =>
            if p.id == winnerPlayer.id then paidWinner
            else p
          }

        Right(
          game.copy(
            players = finalPlayers,
            pot = 0,
            phase = PokerPhase.Complete,
            currentPlayerId = "",
            winnerPlayerId = Some(winnerPlayer.id),
            winnerPlayerIds = List(winnerPlayer.id),
            winningHand = None,
            lastAction =
              Some(s"${player.name} folded")
          )
        )

  private def resolveShowdown(
                               game: PokerGame
                             ): Either[String, PokerGame] =

    val activePlayers =
      game.players.filter(!_.folded)

    if activePlayers.isEmpty then
      Left("No active players remain")

    else if activePlayers.size == 1 then
      Left("Showdown requires at least two players")

    else

      val playerValues =
        activePlayers.map { player =>

          val value =
            PokerHandEvaluator.bestOfSeven(
              player.holeCards,
              game.communityCards
            )

          player.id -> value
        }

      val bestValue =
        playerValues
          .map(_._2)
          .max

      val winners =
        playerValues
          .filter { case (_, value) =>
            value == bestValue
          }
          .map(_._1)

      val share =
        game.pot / winners.size

      val remainder =
        game.pot % winners.size

      val updatedPlayers =
        game.players.map { player =>

          if winners.contains(player.id) then

            val payout =
              share +
                (if player.id == winners.head
                then remainder
                else 0)

            player.copy(
              chips = player.chips + payout
            )

          else
            player
        }

      Right(
        game.copy(
          players = updatedPlayers,
          pot = 0,
          phase = PokerPhase.Complete,
          currentPlayerId = "",
          winnerPlayerId =
            if winners.size == 1
            then Some(winners.head)
            else None,
          winnerPlayerIds = winners,
          winningHand =
            Some(bestValue.categoryName),
          lastAction =
            Some(
              if winners.size == 1 then
                s"${bestValue.categoryName} wins"
              else
                s"${bestValue.categoryName} - split pot"
            )
        )
      )
      
  private def runoutToShowdown(
                                game: PokerGame
                              ): Either[String, PokerGame] =

    val cardsNeeded =
      5 - game.communityCards.length

    if game.deck.length < cardsNeeded then
      Left("Not enough cards remaining for the board")

    else

      val newCommunityCards =
        game.communityCards ++
          game.deck.take(cardsNeeded)

      val remainingDeck =
        game.deck.drop(cardsNeeded)

      resolveShowdown(
        game.copy(
          deck = remainingDeck,
          communityCards = newCommunityCards
        )
      )

  private def finishAction(
                            game: PokerGame
                          ): Either[String, PokerGame] =

    val activePlayers =
      game.players.filter(!_.folded)

    if activePlayers.size == 1 then
      fold(game, game.players.find(_.folded).get)

    else if activePlayers.forall(_.allIn) then
      runoutToShowdown(game)

    else
      val highestBet =
        activePlayers.map(_.currentBet).max

      val updatedActedPlayerIds =
        game.actedPlayerIds + game.currentPlayerId

      val bettingRoundComplete =
        activePlayers.forall { player =>
          player.allIn ||
            (
              updatedActedPlayerIds.contains(player.id) &&
                player.currentBet == highestBet
              )
        }

      if bettingRoundComplete then
        advanceStreet(
          game.copy(
            actedPlayerIds = updatedActedPlayerIds
          )
        )
      else
        Right(
          game.copy(
            actedPlayerIds = updatedActedPlayerIds,
            currentPlayerId =
              nextPlayerId(
                game,
                game.currentPlayerId
              )
          )
        )


  private def nextPlayerId(
                            game: PokerGame,
                            currentPlayerId: String
                          ): String =

    game.players
      .filter(player =>
        !player.folded &&
          !player.allIn &&
          player.id != currentPlayerId
      )
      .headOption
      .map(_.id)
      .getOrElse("")


  private def advanceStreet(
                             game: PokerGame
                           ): Either[String, PokerGame] =

    game.phase match

      case PokerPhase.PreFlop =>
        val flop =
          game.deck.take(3)

        val remainingDeck =
          game.deck.drop(3)

        val resetPlayers =
          game.players.map { p =>
            p.copy(currentBet = 0)
          }

        Right(
          game.copy(
            players = resetPlayers,
            deck = remainingDeck,
            communityCards = flop,
            phase = PokerPhase.Flop,
            currentPlayerId = game.dealerPlayerId,
            minimumRaise = game.bigBlind,
            actedPlayerIds = Set.empty,
            lastAction = Some("The flop was dealt")
          )
        )

      case PokerPhase.Flop =>
        val turn =
          game.deck.head

        val remainingDeck =
          game.deck.tail

        val resetPlayers =
          game.players.map { p =>
            p.copy(currentBet = 0)
          }

        Right(
          game.copy(
            players = resetPlayers,
            deck = remainingDeck,
            communityCards =
              game.communityCards :+ turn,
            phase = PokerPhase.Turn,
            currentPlayerId = game.dealerPlayerId,
            minimumRaise = game.bigBlind,
            actedPlayerIds = Set.empty,
            lastAction = Some("The turn was dealt")
          )
        )

      case PokerPhase.Turn =>
        val river =
          game.deck.head

        val remainingDeck =
          game.deck.tail

        val resetPlayers =
          game.players.map { p =>
            p.copy(currentBet = 0)
          }

        Right(
          game.copy(
            players = resetPlayers,
            deck = remainingDeck,
            communityCards =
              game.communityCards :+ river,
            phase = PokerPhase.River,
            currentPlayerId = game.dealerPlayerId,
            minimumRaise = game.bigBlind,
            actedPlayerIds = Set.empty,
            lastAction = Some("The river was dealt")
          )
        )

      case PokerPhase.River =>
        resolveShowdown(
          game.copy(
            lastAction =
              Some("The hand reached showdown")
          )
        )

      case PokerPhase.Showdown =>
        Right(game)

      case PokerPhase.Complete =>
        Right(game)


  private def recordAction(
                            game: PokerGame,
                            player: PokerPlayer,
                            action: PokerAction,
                            potBefore: Int
                          ): PokerGame =
    game.copy(
      actionHistory =
        game.actionHistory :+
          PokerActionRecord(
            playerId = player.id,
            phase = game.phase,
            action = action,
            communityCards = game.communityCards,
            potBefore = potBefore
          )
    )