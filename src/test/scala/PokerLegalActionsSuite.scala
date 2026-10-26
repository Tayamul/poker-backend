
import munit.FunSuite

class PokerLegalActionsSuite extends FunSuite:
  test("pre-flop actions expose call and raise limits"):
    val game = PokerGame.start(
      startingChips = 1000,
      smallBlind = 5,
      bigBlind = 10
    )

    val legal =
      PokerLegalActions.calculate(game, game.currentPlayerId)

    assert(legal.actions.contains("fold"))
    assert(legal.actions.contains("call"))
    assert(legal.actions.contains("raise"))
    assertEquals(legal.toCall, 5)
    assertEquals(legal.minRaiseTo, Some(20))
    assert(legal.maxRaiseTo.isDefined)

  test("a player who is not up cannot act"):
    val game = PokerGame.start()
    val otherPlayerId =
      game.players.find(_.id != game.currentPlayerId).get.id

    val legal =
      PokerLegalActions.calculate(game, otherPlayerId)

    assertEquals(legal.actions, Nil)
    assertEquals(legal.minRaiseTo, None)
    assertEquals(legal.maxRaiseTo, None)

  test("a finished hand exposes no legal actions"):
    val game =
      PokerGame.start().copy(phase = PokerPhase.Complete)

    val legal =
      PokerLegalActions.calculate(game, game.currentPlayerId)

    assertEquals(legal.actions, Nil)
