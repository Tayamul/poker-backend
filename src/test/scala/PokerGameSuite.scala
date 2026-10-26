import munit.FunSuite

class PokerGameSuite extends FunSuite:

  private val playerConfigs = List(
    PokerPlayerConfig("a", "Player A", 1000),
    PokerPlayerConfig("b", "Player B", 1000),
    PokerPlayerConfig("c", "Player C", 1000),
    PokerPlayerConfig("d", "Player D", 1000)
  )

  private def gameState(
                         currentPlayerId: String = "a",
                         minimumRaise: Int = 100,
                         actedPlayerIds: Set[String] = Set.empty,
                         raiseAllowedPlayerIds: Set[String] = Set("a", "b", "c", "d"),
                         bets: Map[String, Int] = Map.empty,
                         stacks: Map[String, Int] = Map.empty,
                         allInPlayerIds: Set[String] = Set.empty,
                         lastActedBetByPlayer: Map[String, Int] = Map.empty
                       ): PokerGame =

    val started = PokerGame.start(
      playerConfigs = playerConfigs,
      smallBlind = 5,
      bigBlind = 10,
      dealerPlayerId = "a"
    )

    val players = started.players.map { player =>
      val bet = bets.getOrElse(
        player.id,
        if player.id == currentPlayerId then 0 else 100
      )
      val chips = stacks.getOrElse(player.id, 900)

      player.copy(
        chips = chips,
        currentBet = bet,
        allIn = allInPlayerIds.contains(player.id),
        folded = false,
        totalContribution = bet
      )
    }

    val board = started.deck.take(3)
    val remainingDeck = started.deck.drop(3)

    started.copy(
      players = players,
      deck = remainingDeck,
      communityCards = board,
      pot = players.map(_.currentBet).sum,
      phase = PokerPhase.Flop,
      currentPlayerId = currentPlayerId,
      minimumRaise = minimumRaise,
      actedPlayerIds = actedPlayerIds,
      raiseAllowedPlayerIds = raiseAllowedPlayerIds,
      lastActedBetByPlayer = lastActedBetByPlayer
    )

  private def player(game: PokerGame, id: String): PokerPlayer =
    game.players.find(_.id == id).get

  test("rejects a raise when raising rights have not been reopened"):

    val game = gameState(
      currentPlayerId = "b",
      actedPlayerIds = Set("a", "b"),
      raiseAllowedPlayerIds = Set("c", "d")
    )

    val result = PokerGame.applyAction(game, PokerAction.Raise(200))

    assert(result.isLeft)

  test("a short all-in raise does not reopen rights by itself"):
    val game = gameState(
      currentPlayerId = "c",
      minimumRaise = 100,
      actedPlayerIds = Set("a", "b"),
      raiseAllowedPlayerIds = Set("c", "d"),
      bets = Map(
        "a" -> 100,
        "b" -> 100,
        "c" -> 0,
        "d" -> 100
      ),
      stacks = Map(
        "a" -> 900,
        "b" -> 900,
        "c" -> 125,
        "d" -> 900
      )
    )

    val result = PokerGame.applyAction(game, PokerAction.Raise(125))

    assert(
      result.isRight,
      s"Expected the short all-in raise to succeed, got: ${result.left.toOption}"
    )

    val updated = result.toOption.get

    assertEquals(player(updated, "c").currentBet, 125)
    assert(player(updated, "c").allIn)
    assert(!updated.raiseAllowedPlayerIds.contains("a"))
    assert(!updated.raiseAllowedPlayerIds.contains("b"))

  test("a full raise reopens raising rights for other eligible players"):
    val game = gameState(
      currentPlayerId = "c",
      minimumRaise = 100,
      actedPlayerIds = Set("a", "b"),
      raiseAllowedPlayerIds = Set("c", "d"),
      bets = Map(
        "a" -> 100,
        "b" -> 100,
        "c" -> 100,
        "d" -> 175
      ),
      stacks = Map(
        "a" -> 900,
        "b" -> 900,
        "c" -> 100,
        "d" -> 825
      ),
      lastActedBetByPlayer = Map(
        "a" -> 100,
        "b" -> 100
      )
    )

    val result = PokerGame.applyAction(game, PokerAction.Raise(200))

    assert(
      result.isRight,
      s"Expected the cumulative short all-in raise to succeed, got: ${result.left.toOption}"
    )

    val updated = result.toOption.get

    assertEquals(player(updated, "c").currentBet, 200)
    assert(player(updated, "c").allIn)
    assert(updated.raiseAllowedPlayerIds.contains("a"))
    assert(updated.raiseAllowedPlayerIds.contains("b"))

  test("a full raise reopens raising rights for other eligible players"):
    val game = gameState(
      currentPlayerId = "a",
      minimumRaise = 100,
      actedPlayerIds = Set("a"),
      raiseAllowedPlayerIds = Set("a", "b", "c", "d"),
      bets = Map(
        "a" -> 0,
        "b" -> 100,
        "c" -> 100,
        "d" -> 100
      ),
      stacks = Map(
        "a" -> 200,
        "b" -> 900,
        "c" -> 900,
        "d" -> 900
      )
    )

    val result =
      PokerGame.applyAction(game, PokerAction.Raise(200))

    assert(
      result.isRight,
      s"Expected a full raise to succeed, got: ${result.left.toOption}"
    )

    val updated = result.toOption.get

    assertEquals(player(updated, "a").currentBet, 200)
    assertEquals(updated.minimumRaise, 100)
    assertEquals(updated.actedPlayerIds, Set("a"))
    assertEquals(
      updated.raiseAllowedPlayerIds,
      Set("b", "c", "d")
    )

  test("resets raising rights when a new street begins"):

    val game = gameState(
      currentPlayerId = "a",
      actedPlayerIds = Set("a", "b", "c", "d"),
      raiseAllowedPlayerIds = Set.empty,
      bets = Map("a" -> 100, "b" -> 100, "c" -> 100, "d" -> 100)
    )

    val result = PokerGame.applyAction(game, PokerAction.Check)

    assert(result.isRight)

    val updated = result.toOption.get

    if updated.phase != PokerPhase.Complete then
      assert(
        updated.raiseAllowedPlayerIds.nonEmpty,
        "Eligible players should have raising rights on a new street"
      )

  test("automatically runs out the board when all players are all-in at start"):

    val game = PokerGame.start(
      playerConfigs = List(
        PokerPlayerConfig("a", "Player A", 5),
        PokerPlayerConfig("b", "Player B", 5)
      ),
      smallBlind = 5,
      bigBlind = 10,
      dealerPlayerId = "a"
    )

    assertEquals(game.phase, PokerPhase.Complete)
    assertEquals(game.communityCards.length, 5)
    assertEquals(game.pot, 0)
    assert(game.winnerPlayerIds.nonEmpty)

