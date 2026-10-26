
import munit.FunSuite

class PokerRoomManagerSuite extends FunSuite:

  test("a player cannot vote to restart before the session ends") {
    val (room, hostCredentials) =
      PokerRoomManager.createRoom("Host").toOption.get

    val result = PokerRoomManager.voteToRestart(
      room.code,
      hostCredentials.playerId,
      hostCredentials.playerToken
    )

    assert(result.isLeft)
    assertEquals(
      result.swap.toOption.get,
      "The session has not ended yet."
    )
  }

  test("an invalid player token cannot vote to restart") {
    val (room, credentials) =
      PokerRoomManager.createRoom("Host").toOption.get

    PokerRoomManager.markSessionOverForTests(room.code)

    val result = PokerRoomManager.voteToRestart(
      room.code,
      credentials.playerId,
      "invalid-token"
    )

    assert(result.isLeft)
    assertEquals(
      result.swap.toOption.get,
      "Invalid player session."
    )
  }

  test("one player's vote does not restart the session") {
    val (room, hostCredentials) =
      PokerRoomManager.createRoom("Host").toOption.get

    val (_, guestCredentials) =
      PokerRoomManager.joinRoom(room.code, "Guest").toOption.get

    PokerRoomManager.markSessionOverForTests(room.code)

    val result = PokerRoomManager.voteToRestart(
      room.code,
      hostCredentials.playerId,
      hostCredentials.playerToken
    )

    assert(result.isRight)

    val updatedRoom = result.toOption.get
    assert(updatedRoom.sessionOver)
    assertEquals(updatedRoom.restartVotes, Set(hostCredentials.playerId))
    assertEquals(updatedRoom.handNumber, room.handNumber)
    assertEquals(updatedRoom.players.map(_.id).toSet,
      Set(hostCredentials.playerId, guestCredentials.playerId))
  }

  test("unanimous votes restart the session with fresh chips and preserve credentials") {
    val (room, hostCredentials) =
      PokerRoomManager.createRoom("Host").toOption.get

    val (_, guestCredentials) =
      PokerRoomManager.joinRoom(room.code, "Guest").toOption.get

    PokerRoomManager.markSessionOverForTests(room.code)

    // The host votes first. The session must remain over.
    val firstVote = PokerRoomManager.voteToRestart(
      room.code,
      hostCredentials.playerId,
      hostCredentials.playerToken
    )

    assert(firstVote.isRight)
    assert(firstVote.toOption.get.sessionOver)

    // The guest's vote completes the unanimous vote.
    val finalVote = PokerRoomManager.voteToRestart(
      room.code,
      guestCredentials.playerId,
      guestCredentials.playerToken
    )

    assert(finalVote.isRight)

    val restartedRoom = finalVote.toOption.get

    assert(!restartedRoom.sessionOver)
    assert(restartedRoom.started)
    assertEquals(restartedRoom.handNumber, 1)
    assertEquals(restartedRoom.restartVotes, Set.empty[String])
    assertEquals(restartedRoom.eliminatedPlayerIds, Set.empty[String])
    assertEquals(restartedRoom.revealDeadlineEpochMillis, None)
    assertEquals(restartedRoom.revealedPlayerIds, Set.empty[String])
    assertEquals(restartedRoom.sessionWinnerPlayerId, None)

    val game = restartedRoom.game.get
    assertEquals(game.players.map(_.chips).sorted, List(990, 995))
    assertEquals(game.players.map(_.id).toSet,
      Set(hostCredentials.playerId, guestCredentials.playerId))

    // Room identity and player credentials must remain unchanged.
    val roomWithBothPlayers =
      PokerRoomManager.getRoom(room.code).get

    // Use this snapshot to check identities and tokens after restarting.
    assertEquals(restartedRoom.code, roomWithBothPlayers.code)
    assertEquals(restartedRoom.hostPlayerId, roomWithBothPlayers.hostPlayerId)
    assertEquals(
      restartedRoom.players.map(p => (p.id, p.token)),
      roomWithBothPlayers.players.map(p => (p.id, p.token))
    )
  }


