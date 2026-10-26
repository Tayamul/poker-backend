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

case class PokerPlayerConfig(
                              id: String,
                              name: String,
                              startingChips: Int
                            )

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
                      raiseAllowedPlayerIds: Set[String] = Set.empty,
                      actionHistory: List[PokerActionRecord] = Nil,
                      lastActedBetByPlayer: Map[String, Int] = Map.empty
                    )


object PokerGame:

  def start(
             startingChips: Int = 1000,
             smallBlind: Int = 5,
             bigBlind: Int = 10
           ): PokerGame =
    start(
      playerConfigs = List(
        PokerPlayerConfig("player1", "You", startingChips),
        PokerPlayerConfig("player2", "Opponent", startingChips)
      ),
      smallBlind = smallBlind,
      bigBlind = bigBlind,
      dealerPlayerId = "player1"
    )

  def start(
             playerConfigs: List[PokerPlayerConfig],
             smallBlind: Int,
             bigBlind: Int,
             dealerPlayerId: String
           ): PokerGame =
    require(playerConfigs.size >= 2, "At least two players are required")
    require(
      playerConfigs.forall(_.id.trim.nonEmpty),
      "Player IDs cannot be empty"
    )
    require(
      playerConfigs.map(_.id).distinct.size == playerConfigs.size,
      "Player IDs must be unique"
    )
    require(
      playerConfigs.forall(_.startingChips > 0),
      "Starting chips must be positive"
    )
    require(
      smallBlind > 0,
      "Small blind must be positive"
    )
    require(
      bigBlind >= smallBlind,
      "Big blind must be at least the small blind"
    )
    require(
      playerConfigs.exists(_.id == dealerPlayerId),
      "Dealer must be one of the players"
    )

    val shuffledDeck = new Deck().shuffle()
    val playerCount = playerConfigs.size

    require(
      playerCount * 2 + 5 <= shuffledDeck.size,
      "Not enough cards to start the hand"
    )

    // Deal one card to each player, then a second card to each.
    val dealtPlayers = playerConfigs.zipWithIndex.map {
      case (config, index) =>
        val blind1 = shuffledDeck(index)
        val blind2 = shuffledDeck(index + playerCount)

        PokerPlayer(
          id = config.id,
          name = config.name,
          chips = config.startingChips,
          holeCards = List(blind1, blind2),
          currentBet = 0,
          folded = false,
          allIn = false,
          totalContribution = 0
        )
    }

    val remainingDeck = shuffledDeck.drop(playerCount * 2)
    val dealerIndex = playerConfigs.indexWhere(_.id == dealerPlayerId)

    def seatAfter(index: Int): Int =
      (index + 1) % playerCount

    // Heads-up: the dealer posts the small blind.
    // 3+ players: the small blind is immediately left of the dealer.
    val smallBlindIndex =
      if playerCount == 2 then dealerIndex
      else seatAfter(dealerIndex)

    val bigBlindIndex = seatAfter(smallBlindIndex)

    def postBlind(
                   player: PokerPlayer,
                   blindAmount: Int
                 ): PokerPlayer =
      val posted = math.min(player.chips, blindAmount)

      player.copy(
        chips = player.chips - posted,
        currentBet = posted,
        allIn = player.chips == posted,
        totalContribution = posted
      )

    val withSmallBlind = dealtPlayers.updated(
      smallBlindIndex,
      postBlind(dealtPlayers(smallBlindIndex), smallBlind)
    )

    val withBlinds = withSmallBlind.updated(
      bigBlindIndex,
      postBlind(withSmallBlind(bigBlindIndex), bigBlind)
    )

    val firstToAct =
      (1 to playerCount)
        .iterator
        .map(offset => withBlinds((bigBlindIndex + offset) % playerCount))
        .find(p => !p.folded && !p.allIn)
        .map(_.id)
        .getOrElse("")

    val initialGame = PokerGame(
      players = withBlinds,
      deck = remainingDeck,
      communityCards = Nil,
      pot =
        withBlinds(smallBlindIndex).currentBet +
          withBlinds(bigBlindIndex).currentBet,
      dealerPlayerId = dealerPlayerId,
      smallBlind = smallBlind,
      bigBlind = bigBlind,
      phase = PokerPhase.PreFlop,
      currentPlayerId = firstToAct,
      minimumRaise = bigBlind,
      raiseAllowedPlayerIds =
        withBlinds
          .filterNot(p => p.folded || p.allIn)
          .map(_.id)
          .toSet
    )

    // If blind posting leaves every live player all-in, there is no betting
    // action to wait for. Deal the board and settle the hand immediately.
    if withBlinds.filterNot(_.folded).forall(_.allIn) then
      runoutToShowdown(initialGame).getOrElse(initialGame)
    else
      initialGame


  def applyAction(
                   game: PokerGame,
                   action: PokerAction
                 ): Either[String, PokerGame] =

    val potBefore = game.pot

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
            game.copy(
              raiseAllowedPlayerIds =
                game.raiseAllowedPlayerIds - currentPlayer.id,
              lastActedBetByPlayer =
                game.lastActedBetByPlayer.updated(currentPlayer.id, highestBet)
            ),
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
                  Some(s"${currentPlayer.name} checked"),
                raiseAllowedPlayerIds =
                  game.raiseAllowedPlayerIds - currentPlayer.id,
                lastActedBetByPlayer =
                  game.lastActedBetByPlayer.updated(currentPlayer.id, highestBet)
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
              allIn = currentPlayer.chips - actualAmount == 0,
              totalContribution = currentPlayer.totalContribution + actualAmount
            )

          val updatedPlayers =
            game.players.map { p =>
              if p.id == currentPlayer.id then updatedPlayer
              else p
            }

          val updatedPot =
            game.pot + actualAmount

          val updatedGame =
            recordAction(
              game.copy(
                players = updatedPlayers,
                pot = updatedPot,
                lastAction =
                  Some(s"${currentPlayer.name} called $actualAmount"),
                raiseAllowedPlayerIds =
                  game.raiseAllowedPlayerIds - currentPlayer.id,
                lastActedBetByPlayer =
                  game.lastActedBetByPlayer.updated(currentPlayer.id, highestBet)
              ),
              currentPlayer,
              action,
              potBefore
            )

          finishAction(updatedGame)

      case PokerAction.Bet(amount) =>
        if amount <= 0 then
          Left("Bet amount must be positive")
        else if highestBet != 0 then
          Left("A bet has already been made. You must call or raise.")
        else if amount < game.bigBlind && amount != currentPlayer.chips then
          Left(s"The minimum bet is ${game.bigBlind}")
        else if amount > currentPlayer.chips then
          Left("You do not have enough chips for that bet")
        else
          val updatedPlayer =
            currentPlayer.copy(
              chips = currentPlayer.chips - amount,
              currentBet = amount,
              allIn = currentPlayer.chips - amount == 0,
              totalContribution = currentPlayer.totalContribution + amount
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
                minimumRaise =
                  if amount >= game.bigBlind then amount
                  else game.bigBlind,
                actedPlayerIds = Set(currentPlayer.id),
                lastActedBetByPlayer =
                  game.lastActedBetByPlayer.updated(currentPlayer.id, amount),
                lastAction =
                  Some(s"${currentPlayer.name} bet $amount"),
                raiseAllowedPlayerIds =
                  game.players
                    .filter(p => p.id != currentPlayer.id && !p.folded && !p.allIn)
                    .map(_.id)
                    .toSet
              ),
              currentPlayer,
              action,
              potBefore
            )

          finishAction(updatedGame)

      case PokerAction.Raise(amount) =>
        val chipsRequired =
          amount - currentPlayer.currentBet

        val minimumTotal =
          highestBet + game.minimumRaise

        val isAllIn =
          chipsRequired == currentPlayer.chips

        val isFullRaise =
          amount >= minimumTotal

        val isShortAllInRaise =
          isAllIn &&
            amount > highestBet &&
            !isFullRaise

        if highestBet <= currentPlayer.currentBet then
          Left("There is no bet to raise")
        else if amount <= highestBet then
          Left("A raise must exceed the current highest bet")
        else if chipsRequired <= 0 then
          Left("Raise amount must exceed your current bet")
        else if chipsRequired > currentPlayer.chips then
          Left("You do not have enough chips for that raise")
        else if !isFullRaise && !isShortAllInRaise then
          Left(s"Minimum raise is to $minimumTotal")
        else if !game.raiseAllowedPlayerIds.contains(currentPlayer.id) then
          Left("You cannot raise because raising rights have not been reopened")
        else
          val eligibleOtherPlayers =
            game.players
              .filter(p =>
                p.id != currentPlayer.id &&
                  !p.folded &&
                  !p.allIn
              )
              .map(_.id)
              .toSet

          // A short all-in does not itself reopen betting. However, multiple
          // short all-ins can cumulatively reach a full raise since a player
          // last acted; in that case, that player's raising rights reopen.
          val cumulativelyReopenedPlayers =
            if isFullRaise then eligibleOtherPlayers
            else
              game.actedPlayerIds
                .filter(_ != currentPlayer.id)
                .filter { playerId =>
                  val lastFacedBet =
                    game.lastActedBetByPlayer.getOrElse(
                      playerId,
                      game.players.find(_.id == playerId).map(_.currentBet).getOrElse(0)
                    )
                  amount - lastFacedBet >= game.minimumRaise
                }
                .filter { playerId => eligibleOtherPlayers.contains(playerId) }

          val updatedRaiseAllowedPlayerIds =
            if isFullRaise then eligibleOtherPlayers
            else
              (game.raiseAllowedPlayerIds - currentPlayer.id) ++ cumulativelyReopenedPlayers

          val raiseSize =
            amount - highestBet

          val updatedPlayer =
            currentPlayer.copy(
              chips = currentPlayer.chips - chipsRequired,
              currentBet = amount,
              allIn = currentPlayer.chips == chipsRequired,
              totalContribution =
                currentPlayer.totalContribution + chipsRequired
            )

          val updatedActedPlayerIds =
            if isFullRaise then
              Set(currentPlayer.id)
            else
              game.actedPlayerIds

          val updatedGame =
            recordAction(
              game.copy(
                players = game.players.map { p =>
                  if p.id == currentPlayer.id then updatedPlayer
                  else p
                },
                pot = game.pot + chipsRequired,
                minimumRaise =
                  if isFullRaise then raiseSize
                  else game.minimumRaise,
                actedPlayerIds = updatedActedPlayerIds,
                lastActedBetByPlayer =
                  game.lastActedBetByPlayer.updated(currentPlayer.id, amount),
                lastAction =
                  Some(s"${currentPlayer.name} raised to $amount"),
                raiseAllowedPlayerIds = updatedRaiseAllowedPlayerIds
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
        if p.id == player.id then p.copy(folded = true)
        else p
      }

    val updatedGame =
      game.copy(
        players = foldedPlayers,
        lastAction = Some(s"${player.name} folded")
      )

    val remainingPlayers =
      foldedPlayers.filterNot(_.folded)

    if remainingPlayers.isEmpty then
      Left("No player remains")
    else
      finishAction(updatedGame)


  private def resolveShowdown(
                               game: PokerGame
                             ): Either[String, PokerGame] =

    val activePlayers =
      game.players.filterNot(_.folded)

    if activePlayers.isEmpty then
      Left("No active players remain")
    else if activePlayers.size == 1 then
      Left("Showdown requires at least two active players")
    else if game.communityCards.length != 5 then
      Left("Showdown requires five community cards")
    else
      val contributions =
        game.players.map { player =>
          PotManager.Contribution(
            playerId = player.id,
            totalContribution = player.totalContribution,
            folded = player.folded
          )
        }

      val handRanks =
        activePlayers.map { player =>
          player.id -> PokerHandEvaluator.bestOfSeven(
            player.holeCards,
            game.communityCards
          )
        }.toMap

      // Deterministic tie-breaking for odd chips.
      // Replace with seat order left of the dealer when that is available.
      val oddChipOrder =
        game.players.map(_.id)

      PotManager.settle(
        players = contributions,
        handRanks = handRanks,
        oddChipOrder = oddChipOrder,
        ordering = Ordering[HandValue]
      ).map { settlement =>
        val updatedPlayers =
          game.players.map { player =>
            player.copy(
              chips = player.chips +
                settlement.payouts.getOrElse(player.id, 0)
            )
          }

        val bestOverallHand =
          handRanks.values.max

        val playersReceivingPayout =
          settlement.payouts.collect {
            case (playerId, amount) if amount > 0 => playerId
          }.toList

        game.copy(
          players = updatedPlayers,
          pot = 0,
          phase = PokerPhase.Complete,
          currentPlayerId = "",
          winnerPlayerId =
            if playersReceivingPayout.size == 1 then
              playersReceivingPayout.headOption
            else
              None,
          winnerPlayerIds = playersReceivingPayout,
          winningHand = Some(bestOverallHand.categoryName),
          lastAction = Some(
            s"Showdown complete: ${settlement.pots.size} pot(s) settled"
          )
        )
      }


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
      game.players.filterNot(_.folded)

    if activePlayers.isEmpty then
      Left("No active players remain")

    else if activePlayers.size == 1 then
      val winner = activePlayers.head
      val paidWinner =
        winner.copy(chips = winner.chips + game.pot)

      Right(
        game.copy(
          players = game.players.map { p =>
            if p.id == winner.id then paidWinner else p
          },
          pot = 0,
          phase = PokerPhase.Complete,
          currentPlayerId = "",
          winnerPlayerId = Some(winner.id),
          winnerPlayerIds = List(winner.id),
          winningHand = None,
          lastAction = Some(s"${winner.name} wins uncontested")
        )
      )

    else
      val highestBet =
        activePlayers.map(_.currentBet).max

      val updatedActedPlayerIds =
        if game.currentPlayerId.isEmpty then game.actedPlayerIds
        else game.actedPlayerIds + game.currentPlayerId

      val bettingRoundComplete =
        activePlayers.forall { player =>
          player.allIn ||
            (
              updatedActedPlayerIds.contains(player.id) &&
                player.currentBet == highestBet
              )
        }

      if !bettingRoundComplete then
        val nextPlayer =
          nextPlayerId(game, game.currentPlayerId)

        if nextPlayer.isEmpty then
          Left("No eligible player can act, but betting is incomplete")
        else
          Right(
            game.copy(
              actedPlayerIds = updatedActedPlayerIds,
              currentPlayerId = nextPlayer
            )
          )

      else
        val playersWhoCanAct =
          activePlayers.filterNot(_.allIn)

        val updatedGame =
          game.copy(actedPlayerIds = updatedActedPlayerIds)

        if playersWhoCanAct.size <= 1 then
          runoutToShowdown(updatedGame)
        else
          advanceStreet(updatedGame)


  private def nextPlayerId(
                            game: PokerGame,
                            currentPlayerId: String
                          ): String =

    val playerCount = game.players.length
    val currentIndex =
      game.players.indexWhere(_.id == currentPlayerId)

    if playerCount == 0 || currentIndex < 0 then
      ""
    else
      (1 to playerCount)
        .iterator
        .map(offset =>
          game.players((currentIndex + offset) % playerCount)
        )
        .find(player => !player.folded && !player.allIn)
        .map(_.id)
        .getOrElse("")

  private def firstToActLeftOfDealer(game: PokerGame): String =
    val count = game.players.length
    val dealerIndex =
      game.players.indexWhere(_.id == game.dealerPlayerId)

    if count == 0 || dealerIndex < 0 then ""
    else
      (1 to count)
        .iterator
        .map(offset => game.players((dealerIndex + offset) % count))
        .find(p => !p.folded && !p.allIn)
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
            currentPlayerId = firstToActLeftOfDealer(game),
            minimumRaise = game.bigBlind,
            actedPlayerIds = Set.empty,
            lastActedBetByPlayer = Map.empty,
            raiseAllowedPlayerIds =
              resetPlayers
                .filter(p => !p.folded && !p.allIn)
                .map(_.id)
                .toSet,
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
            currentPlayerId = firstToActLeftOfDealer(game),
            minimumRaise = game.bigBlind,
            actedPlayerIds = Set.empty,
            lastActedBetByPlayer = Map.empty,
            lastAction = Some("The turn was dealt"),
            raiseAllowedPlayerIds =
              resetPlayers
                .filter(p => !p.folded && !p.allIn)
                .map(_.id)
                .toSet
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
            currentPlayerId = firstToActLeftOfDealer(game),
            minimumRaise = game.bigBlind,
            actedPlayerIds = Set.empty,
            lastActedBetByPlayer = Map.empty,
            lastAction = Some("The river was dealt"),
            raiseAllowedPlayerIds =
              resetPlayers
                .filter(p => !p.folded && !p.allIn)
                .map(_.id)
                .toSet
          )
        )

      case PokerPhase.River =>
        resolveShowdown(
          game.copy(
            lastAction = Some("The hand reached showdown")
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
