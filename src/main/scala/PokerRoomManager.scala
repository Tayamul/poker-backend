import java.security.SecureRandom
import java.util.concurrent.{Executors, ThreadFactory, TimeUnit}
import java.util.UUID
import scala.collection.mutable

case class PokerRoomPlayer(
                            id: String,
                            name: String,
                            token: String
                          )


case class PokerRoom(
                      code: String,
                      hostPlayerId: String,
                      players: List[PokerRoomPlayer],
                      started: Boolean = false,
                      game: Option[PokerGame] = None,
                      revealDeadlineEpochMillis: Option[Long] = None,
                      revealedPlayerIds: Set[String] = Set.empty,
                      sessionOver: Boolean = false,
                      sessionWinnerPlayerId: Option[String] = None,
                      handNumber: Int = 0,
                      eliminatedPlayerIds: Set[String] = Set.empty,
                      restartVotes: Set[String] = Set.empty
                    )


case class PokerRoomCredentials(
                                 roomCode: String,
                                 playerId: String,
                                 playerToken: String
                               )

object PokerRoomManager:

  private val random = new SecureRandom()
  private val rooms = mutable.Map.empty[String, PokerRoom]
  private val scheduler =
    Executors.newSingleThreadScheduledExecutor(
      new ThreadFactory:
        override def newThread(runnable: Runnable): Thread =
          val thread = new Thread(runnable, "poker-session-timer")
          thread.setDaemon(true)
          thread
    )

  val MinPlayers = 2
  val MaxPlayers = 4

  def markSessionOverForTests(code: String): Unit = synchronized {
    rooms.get(code).foreach { room =>
      rooms(code) = room.copy(
        sessionOver = true,
        sessionWinnerPlayerId = Some(room.hostPlayerId)
      )
    }
  }

  private def newToken(): String =
    UUID.randomUUID().toString

  private def newPlayerId(): String =
    UUID.randomUUID().toString


  private def newRoomCode(): String =
    var code = f"${random.nextInt(1000000)}%06d"
    while rooms.contains(code) do
      code = f"${random.nextInt(1000000)}%06d"
    code


  private def cleanName(name: String): Either[String, String] =
    val cleaned = Option(name).getOrElse("").trim
    if cleaned.isEmpty then
      Left("Player name cannot be empty")
    else if cleaned.length > 24 then
      Left("Player name must be 24 characters or fewer")
    else
      Right(cleaned)


  private val RevealWindowMillis = 25000L

  private def isGameOver(game: PokerGame): Boolean =
    game.phase == PokerPhase.Showdown ||
      game.phase == PokerPhase.Complete

  private def withRevealWindow(
                                previousGame: PokerGame,
                                updatedRoom: PokerRoom,
                                updatedGame: PokerGame
                              ): PokerRoom =
    if !isGameOver(previousGame) && isGameOver(updatedGame) then
      updatedRoom.copy(
        revealDeadlineEpochMillis =
          Some(System.currentTimeMillis() + RevealWindowMillis),
        revealedPlayerIds = Set.empty
      )
    else updatedRoom


  private def withSessionOutcome(
                                  room: PokerRoom,
                                  game: PokerGame
                                ): PokerRoom =
    val playersWithChips = game.players.filter(_.chips > 0)
    val playersWithChipsIds = playersWithChips.map(_.id).toSet

    room.copy(
      sessionOver = playersWithChips.size <= 1,
      sessionWinnerPlayerId =
        if playersWithChips.size == 1 then Some(playersWithChips.head.id)
        else None,
      eliminatedPlayerIds =
        room.players.map(_.id).toSet -- playersWithChipsIds
    )

  private def scheduleNextHand(room: PokerRoom): Unit =
    room.revealDeadlineEpochMillis.foreach { deadline =>
      scheduler.schedule(
        new Runnable:
          override def run(): Unit =
            advanceAfterReveal(room.code, deadline), RevealWindowMillis, TimeUnit.MILLISECONDS
      )
    }

  private def advanceAfterReveal(
                                  code: String,
                                  expectedDeadline: Long
                                ): Unit = synchronized {
    rooms.get(code).foreach { room =>
      val deadlineMatches =
        room.revealDeadlineEpochMillis.contains(expectedDeadline)

      val revealWindowExpired =
        System.currentTimeMillis() >= expectedDeadline

      if room.started &&
        !room.sessionOver &&
        deadlineMatches &&
        revealWindowExpired &&
        room.game.exists(isGameOver)
      then
        val currentGame = room.game.get
        val activePlayers = currentGame.players.filter(_.chips > 0)

        if activePlayers.size <= 1 then
          val updatedRoom = withSessionOutcome(room, currentGame)
          rooms(code) = updatedRoom
        else
          val activeIds = activePlayers.map(_.id).toSet

          val configs = activePlayers.map { player =>
            PokerPlayerConfig(
              id = player.id,
              name = player.name,
              startingChips = player.chips
            )
          }

          val dealerIndex =
            room.players.indexWhere(_.id == currentGame.dealerPlayerId)

          val startingIndex = if dealerIndex >= 0 then dealerIndex else 0

          val nextDealer = (1 to room.players.size)
            .iterator
            .map { offset =>
              room.players((startingIndex + offset) % room.players.size)
            }
            .find(player => activeIds.contains(player.id))
            .get

          val nextGame = PokerGame.start(
            configs,
            currentGame.smallBlind,
            currentGame.bigBlind,
            nextDealer.id
          )

          val updatedRoom = room.copy(
            game = Some(nextGame),
            revealDeadlineEpochMillis = None,
            revealedPlayerIds = Set.empty,
            handNumber = room.handNumber + 1,
            eliminatedPlayerIds =
              room.players.map(_.id).toSet -- activeIds
          )

          rooms(code) = updatedRoom
    }
  }

  def createRoom(
                  hostName: String
                ): Either[String, (PokerRoom, PokerRoomCredentials)] =
    cleanName(hostName).map { name =>
      synchronized {
        val code = newRoomCode()
        val host = PokerRoomPlayer(
          id = newPlayerId(),
          name = name,
          token = newToken()
        )

        val room = PokerRoom(
          code = code,
          hostPlayerId = host.id,
          players = List(host)
        )

        rooms(code) = room

        val credentials = PokerRoomCredentials(
          roomCode = code,
          playerId = host.id,
          playerToken = host.token
        )

        (room, credentials)
      }
    }

  def joinRoom(
                code: String,
                playerName: String
              ): Either[String, (PokerRoom, PokerRoomCredentials)] =
    for
      name <- cleanName(playerName)
      result <- synchronized {
        rooms.get(Option(code).getOrElse("").trim) match
          case None =>
            Left("Room not found. Check the six-digit code.")

          case Some(room) if room.started =>
            Left("This game has already started.")

          case Some(room) if room.players.size >= MaxPlayers =>
            Left("This room is full. A maximum of four players can join.")

          case Some(room) =>
            val player = PokerRoomPlayer(
              id = newPlayerId(),
              name = name,
              token = newToken()
            )

            val updatedRoom =
              room.copy(players = room.players :+ player)

            rooms(room.code) = updatedRoom

            val credentials = PokerRoomCredentials(
              roomCode = room.code,
              playerId = player.id,
              playerToken = player.token
            )

            Right((updatedRoom, credentials))
      }
    yield result

  def getRoom(code: String): Option[PokerRoom] =
    synchronized {
      rooms.get(Option(code).getOrElse("").trim)
    }

  def startRoom(
                 code: String,
                 requestingPlayerId: String,
                 playerToken: String
               ): Either[String, PokerRoom] =
    synchronized {
      rooms.get(Option(code).getOrElse("").trim) match
        case None =>
          Left("Room not found.")

        case Some(room)
          if room.hostPlayerId != requestingPlayerId =>
          Left("Only the host can start the game.")

        case Some(room)
          if !room.players.exists(p =>
            p.id == requestingPlayerId &&
              p.token == playerToken
          ) =>
          Left("Invalid player session.")

        case Some(room) if room.started =>
          Left("The game has already started.")

        case Some(room) if room.players.size < MinPlayers =>
          Left("At least two players must join before starting.")

        case Some(room) =>
          val configs = room.players.map { player =>
            PokerPlayerConfig(
              id = player.id,
              name = player.name,
              startingChips = 1000
            )
          }

          val game = PokerGame.start(
            playerConfigs = configs,
            smallBlind = 5,
            bigBlind = 10,
            dealerPlayerId = room.hostPlayerId
          )

          val updatedRoom = room.copy(
            started = true,
            game = Some(game),
            revealDeadlineEpochMillis = None,
            revealedPlayerIds = Set.empty,
            sessionOver = false,
            sessionWinnerPlayerId = None,
            handNumber = 1,
            eliminatedPlayerIds = Set.empty
          )

          rooms(code) = updatedRoom
          Right(updatedRoom)
    }


  def actInRoom(
                 code: String,
                 playerId: String,
                 playerToken: String,
                 action: PokerAction
               ): Either[String, PokerRoom] =
    synchronized {
      rooms.get(Option(code).getOrElse("").trim) match
        case None =>
          Left("Room not found.")

        case Some(room) if !room.started =>
          Left("The game has not started yet.")

        case Some(room) if !room.players.exists(p =>
          p.id == playerId && p.token == playerToken
        ) =>
          Left("Invalid player session.")

        case Some(room) =>
          room.game match
            case None =>
              Left("Game state is unavailable.")

            case Some(game) if game.currentPlayerId != playerId =>
              Left("It is not your turn.")

            case Some(game) =>
              PokerGame.applyAction(game, action) match
                case Left(error) =>
                  Left(error)


                case Right(updatedGame) =>
                  val handJustFinished =
                    !isGameOver(game) && isGameOver(updatedGame)

                  val roomWithGame =
                    withRevealWindow(
                      game,
                      room.copy(game = Some(updatedGame)),
                      updatedGame
                    )

                  val updatedRoom =
                    if handJustFinished then
                      withSessionOutcome(roomWithGame, updatedGame)
                    else
                      roomWithGame

                  rooms(code) = updatedRoom

                  if handJustFinished && !updatedRoom.sessionOver then
                    scheduleNextHand(updatedRoom)

                  Right(updatedRoom)

    }


  def revealCards(
                   code: String,
                   playerId: String,
                   playerToken: String
                 ): Either[String, PokerRoom] =
    synchronized {
      rooms.get(Option(code).getOrElse("").trim) match
        case None =>
          Left("Room not found.")

        case Some(room)
          if !room.players.exists(p =>
            p.id == playerId && p.token == playerToken
          ) =>
          Left("Invalid player session.")

        case Some(room) if !room.started || room.game.isEmpty =>
          Left("The game has not started.")

        case Some(room) if !isGameOver(room.game.get) =>
          Left("Cards can only be revealed after the hand ends.")

        case Some(room) if room.revealedPlayerIds.contains(playerId) =>
          Right(room)

        case Some(room)
          if room.revealDeadlineEpochMillis.forall(
            deadline => System.currentTimeMillis() > deadline
          ) =>
          Left("The card reveal window has expired.")

        case Some(room) =>
          val updatedRoom = room.copy(
            revealedPlayerIds = room.revealedPlayerIds + playerId
          )
          rooms(room.code) = updatedRoom
          Right(updatedRoom)
    }

  private def restartSession(room: PokerRoom): PokerRoom =
    val configs = room.players.map { player =>
      PokerPlayerConfig(
        id = player.id,
        name = player.name,
        startingChips = 1000
      )
    }

    val game = PokerGame.start(
      playerConfigs = configs,
      smallBlind = 5,
      bigBlind = 10,
      dealerPlayerId = room.hostPlayerId
    )

    room.copy(
      started = true,
      game = Some(game),
      revealDeadlineEpochMillis = None,
      revealedPlayerIds = Set.empty,
      sessionOver = false,
      sessionWinnerPlayerId = None,
      handNumber = 1,
      eliminatedPlayerIds = Set.empty,
      restartVotes = Set.empty
    )

  def voteToRestart(
                     code: String,
                     playerId: String,
                     playerToken: String
                   ): Either[String, PokerRoom] = synchronized {
    val normalizedCode = Option(code).getOrElse("").trim

    rooms.get(normalizedCode) match
      case None =>
        Left("Room not found.")

      case Some(room)
        if !room.players.exists(p =>
          p.id == playerId && p.token == playerToken
        ) =>
        Left("Invalid player session.")

      case Some(room) if !room.sessionOver =>
        Left("The session has not ended yet.")

      case Some(room) =>
        val updatedVotes = room.restartVotes + playerId
        val allPlayerIds = room.players.map(_.id).toSet

        if allPlayerIds.subsetOf(updatedVotes) then
          val restartedRoom = restartSession(room)
          rooms(normalizedCode) = restartedRoom
          Right(restartedRoom)
        else
          val updatedRoom = room.copy(restartVotes = updatedVotes)
          rooms(normalizedCode) = updatedRoom
          Right(updatedRoom)
  }

// Useful for isolated tests; do not expose this as a public API route.
//  private[poker] def clearForTests(): Unit =
//    synchronized {
//      rooms.clear()
//    }
