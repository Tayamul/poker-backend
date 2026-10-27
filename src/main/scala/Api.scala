import scala.annotation.tailrec
import scala.util.Try

object Api extends cask.MainRoutes:

  override def port: Int =
    sys.env.get("PORT").flatMap(_.toIntOption)
      .orElse(sys.props.get("poker.api.port").flatMap(_.toIntOption))
      .getOrElse(8080)

  override def host: String = "0.0.0.0"

  private def allowedOrigin: String =
    sys.env.getOrElse("FRONTEND_ORIGIN", "http://localhost:5173")

  private var currentGame: Option[PokerGame] = None

  def resetGameStateForTests(): Unit =
    currentGame = None
  def initializeForTests(): Unit =
    initialize()

  private def cardJson(card: Card): String =
    s"""{"rank":"${card.rank}","suit":"${card.suit}"}"""


  private def cardsJson(cards: List[Card]): String =
    cards
      .map(cardJson)
      .mkString("[", ",", "]")


  private def jsonResponse(
                            json: String
                          ): cask.Response[String] =
    cask.Response(
      json,
      headers = Seq(
        "Content-Type" -> "application/json",
        "Access-Control-Allow-Origin" ->
          allowedOrigin
      )
    )


  private def errorResponse(
                             message: String,
                             status: Int
                           ): cask.Response[String] =
    cask.Response(
      ujson.write(ujson.Obj("error" -> message)),
      statusCode = status,
      headers = Seq(
        "Content-Type" -> "application/json",
        "Access-Control-Allow-Origin" -> allowedOrigin
      )
    )


  private def onlineRoomJson(
                              room: PokerRoom,
                              viewerPlayerId: String
                            ): String =
    val playersJson =
      room.players.map { player =>
        ujson.Obj(
          "id" -> player.id,
          "name" -> player.name
        )
      }

    val gameValue: ujson.Value =
      room.game match
        case Some(game) =>
          ujson.read(
            gameJson(
              game,
              viewerPlayerId,
              room.revealedPlayerIds,
              online = true
            )
          )
        case None =>
          ujson.Null

    val deadlineValue: ujson.Value =
      room.revealDeadlineEpochMillis match
        case Some(deadline) => ujson.Num(deadline.toDouble)
        case None => ujson.Null

    ujson.write(
      ujson.Obj(
        "code" -> room.code,
        "hostPlayerId" -> room.hostPlayerId,
        "started" -> room.started,
        "players" -> ujson.Arr.from(playersJson),
        "game" -> gameValue,
        "revealDeadlineEpochMillis" -> deadlineValue,
        "revealedPlayerIds" -> ujson.Arr.from(
          room.revealedPlayerIds.toSeq.sorted.map(ujson.Str(_))
        ),
        "sessionOver" -> ujson.Bool(room.sessionOver),
        "sessionWinnerPlayerId" ->
          room.sessionWinnerPlayerId
            .map(ujson.Str(_))
            .getOrElse(ujson.Null),
        "handNumber" -> ujson.Num(room.handNumber),
        "eliminatedPlayerIds" ->
          ujson.Arr.from(
            room.eliminatedPlayerIds.toSeq.map(ujson.Str(_))
          ),
        "restartVotes" -> ujson.Arr.from(
          room.restartVotes.toSeq.sorted.map(ujson.Str(_))
        )
      )
    )


  @cask.options("/api/game/action")
  def gameActionOptions(): cask.Response[String] =
    cask.Response(
      "",
      statusCode = 204,
      headers = Seq(
        "Access-Control-Allow-Origin" -> allowedOrigin,
        "Access-Control-Allow-Methods" -> "POST, OPTIONS",
        "Access-Control-Allow-Headers" -> "Content-Type"
      )
    )

  @cask.get("/")
  def hello(): String =
    "Nepalese Poker API is running!"

  // ------------------------------------------------------------
  // Temporary deck endpoints
  // ------------------------------------------------------------

  @cask.get("/api/deck")
  def deck(): cask.Response[String] =
    val deck = new Deck()
    jsonResponse(cardsJson(deck.cards))


  @cask.get("/api/deck/shuffle")
  def shuffleDeck(): cask.Response[String] =
    val deck = new Deck()
    jsonResponse(cardsJson(deck.shuffle()))

  // ------------------------------------------------------------
  // Poker game
  // ------------------------------------------------------------

  @cask.post("/api/game/start")
  def startGame(): cask.Response[String] =

    val game =
      PokerGame.start()

    currentGame =
      Some(game)

    jsonResponse(gameJson(game))


  @cask.get("/api/game")
  def getGame(): cask.Response[String] =

    currentGame match

      case Some(game) =>
        jsonResponse(gameJson(game))

      case None =>
        cask.Response(
          """{"error":"No game has been started"}""",
          statusCode = 404,
          headers = Seq(
            "Content-Type" -> "application/json",
            "Access-Control-Allow-Origin" ->
              allowedOrigin
          )
        )


  @cask.post("/api/game/action")
  def gameAction(
                  request: cask.Request
                ): cask.Response[String] =

    val parsedRequest = ActionRequestParser.parse(request.text())

    currentGame match

      case None =>
        cask.Response(
          """{"error":"No game has been started"}""",
          statusCode = 404,
          headers = Seq(
            "Content-Type" -> "application/json",
            "Access-Control-Allow-Origin" ->
              allowedOrigin
          )
        )

      case Some(game) =>

        parsedRequest match
          case Left(error) =>
            cask.Response(
              ujson.write(ujson.Obj("error" -> error)),
              statusCode = 400,
              headers = Seq(
                "Content-Type" -> "application/json",
                "Access-Control-Allow-Origin" -> allowedOrigin
              )
            )

          case Right(pokerAction) =>
            PokerGame.applyAction(game, pokerAction) match
              case Left(error) =>
                cask.Response(
                  ujson.write(ujson.Obj("error" -> error)),
                  statusCode = 400,
                  headers = Seq(
                    "Content-Type" -> "application/json",
                    "Access-Control-Allow-Origin" -> allowedOrigin
                  )
                )

              case Right(updatedGame) =>
                playOpponentTurns(updatedGame) match
                  case Left(error) =>
                    cask.Response(
                      ujson.write(ujson.Obj("error" -> error)),
                      statusCode = 400,
                      headers = Seq(
                        "Content-Type" -> "application/json",
                        "Access-Control-Allow-Origin" -> allowedOrigin
                      )
                    )

                  case Right(finalGame) =>
                    currentGame = Some(finalGame)
                    jsonResponse(gameJson(finalGame))

  @cask.route("/api/rooms", methods = Seq("options"))
  def optionsCreateRoom() =
    cask.Response(
      "",
      statusCode = 204,
      headers = Seq(
        "Access-Control-Allow-Origin" -> allowedOrigin,
        "Access-Control-Allow-Methods" -> "POST, OPTIONS",
        "Access-Control-Allow-Headers" -> "Content-Type, X-Player-Id"
      )
    )

  @cask.post("/api/rooms")
  def createOnlineRoom(request: cask.Request): cask.Response[String] =
    val parsed = Try(ujson.read(request.text())).toEither

    parsed match
      case Left(_) =>
        errorResponse("Malformed JSON", 400)

      case Right(obj: ujson.Obj) =>
        obj.value.get("name") match
          case Some(ujson.Str(name)) =>
            PokerRoomManager.createRoom(name) match
              case Left(error) =>
                errorResponse(error, 400)

              case Right((room, credentials)) =>
                jsonResponse(
                  ujson.write(
                    ujson.Obj(
                      "code" -> credentials.roomCode,
                      "playerId" -> credentials.playerId,
                      "playerToken" -> credentials.playerToken,
                      "hostPlayerId" -> room.hostPlayerId,
                      "started" -> room.started,
                      "players" -> ujson.Arr.from(
                        room.players.map { player =>
                          ujson.Obj(
                            "id" -> player.id,
                            "name" -> player.name
                          )
                        }
                      )
                    )
                  )
                )

          case _ =>
            errorResponse("Field 'name' must be a string", 400)

      case Right(_) =>
        errorResponse("Request body must be a JSON object", 400)


  @cask.options("/api/rooms/:code/join")
  def joinRoomOptions(code: String): cask.Response[String] =
    cask.Response("", statusCode = 204, headers = Seq(
      "Access-Control-Allow-Origin" -> allowedOrigin,
      "Access-Control-Allow-Methods" -> "POST, OPTIONS",
      "Access-Control-Allow-Headers" -> "Content-Type, X-Player-Id"
    ))

  @cask.post("/api/rooms/:code/join")
  def joinOnlineRoom(
                      code: String,
                      request: cask.Request
                    ): cask.Response[String] =
    Try(ujson.read(request.text())).toEither match
      case Left(_) =>
        errorResponse("Malformed JSON", 400)

      case Right(obj: ujson.Obj) =>
        obj.value.get("name") match
          case Some(ujson.Str(name)) =>
            PokerRoomManager.joinRoom(code, name) match
              case Left(error) =>
                errorResponse(error, 400)

              case Right((room, credentials)) =>
                jsonResponse(ujson.write(ujson.Obj(
                  "code" -> credentials.roomCode,
                  "playerId" -> credentials.playerId,
                  "playerToken" -> credentials.playerToken,
                  "hostPlayerId" -> room.hostPlayerId,
                  "started" -> room.started,
                  "players" -> ujson.Arr.from(room.players.map { player =>
                    ujson.Obj(
                      "id" -> player.id,
                      "name" -> player.name
                    )
                  })
                )))

          case _ =>
            errorResponse("Field 'name' must be a string", 400)

      case Right(_) =>
        errorResponse("Request body must be a JSON object", 400)


  @cask.options("/api/rooms/:code")
  def getOnlineRoomOptions(code: String): cask.Response[String] =
    cask.Response("", statusCode = 204, headers = Seq(
      "Access-Control-Allow-Origin" -> allowedOrigin,
      "Access-Control-Allow-Methods" -> "GET, OPTIONS",
      "Access-Control-Allow-Headers" -> "Content-Type, X-Player-Id"
    ))


  @cask.get("/api/rooms/:code")
  def getOnlineRoom(
                     code: String,
                     request: cask.Request
                   ): cask.Response[String] =

    val playerId: Option[String] =
      request.headers.get("x-player-id").flatMap(_.headOption)

    playerId match
      case None =>
        errorResponse("Header 'X-Player-Id' is required", 400)

      case Some(id) =>
        PokerRoomManager.getRoom(code) match
          case None =>
            errorResponse("Room not found", 404)

          case Some(room) =>
            if !room.players.exists(_.id == id) then
              errorResponse("Player is not in this room", 403)
            else
              jsonResponse(onlineRoomJson(room, id))


  @cask.options("/api/rooms/:code/start")
  def startOnlineRoomOptions(code: String): cask.Response[String] =
    cask.Response("", statusCode = 204, headers = Seq(
      "Access-Control-Allow-Origin" -> allowedOrigin,
      "Access-Control-Allow-Methods" -> "POST, OPTIONS",
      "Access-Control-Allow-Headers" -> "Content-Type, X-Player-Id"
    ))

  @cask.post("/api/rooms/:code/start")
  def startOnlineRoom(
                       code: String,
                       request: cask.Request
                     ): cask.Response[String] =
    parseRoomCredentials(request) match
      case Left(error) =>
        errorResponse(error, 400)

      case Right((playerId, playerToken)) =>
        PokerRoomManager.startRoom(code, playerId, playerToken) match
          case Left(error) =>
            errorResponse(error, 400)

          case Right(room) =>
            jsonResponse(onlineRoomJson(room, playerId))


  @cask.options("/api/rooms/:code/action")
  def onlineActionOptions(code: String): cask.Response[String] =
    cask.Response("", statusCode = 204, headers = Seq(
      "Access-Control-Allow-Origin" -> allowedOrigin,
      "Access-Control-Allow-Methods" -> "POST, OPTIONS",
      "Access-Control-Allow-Headers" -> "Content-Type, X-Player-Id"
    ))

  @cask.post("/api/rooms/:code/action")
  def onlineAction(
                    code: String,
                    request: cask.Request
                  ): cask.Response[String] =
    Try(ujson.read(request.text())).toEither match
      case Left(_) =>
        errorResponse("Malformed JSON", 400)

      case Right(obj: ujson.Obj) =>
        val playerId = obj.value.get("playerId")
        val playerToken = obj.value.get("playerToken")

        (playerId, playerToken) match
          case (Some(ujson.Str(id)), Some(ujson.Str(token))) =>
            ActionRequestParser.parse(request.text()) match
              case Left(error) =>
                errorResponse(error, 400)

              case Right(action) =>
                PokerRoomManager.actInRoom(code, id, token, action) match
                  case Left(error) =>
                    errorResponse(error, 400)

                  case Right(room) =>
                    jsonResponse(onlineRoomJson(room, id))

          case _ =>
            errorResponse(
              "Fields 'playerId' and 'playerToken' are required",
              400
            )

      case Right(_) =>
        errorResponse("Request body must be a JSON object", 400)


  private def parseRoomCredentials(
                                    request: cask.Request
                                  ): Either[String, (String, String)] =
    Try(ujson.read(request.text())).toEither match
      case Left(_) =>
        Left("Malformed JSON")

      case Right(obj: ujson.Obj) =>
        (obj.value.get("playerId"), obj.value.get("playerToken")) match
          case (Some(ujson.Str(id)), Some(ujson.Str(token))) =>
            Right((id, token))

          case _ =>
            Left("Fields 'playerId' and 'playerToken' are required")

      case Right(_) =>
        Left("Request body must be a JSON object")


  @cask.options("/api/rooms/:code/reveal")
  def revealCardsOptions(code: String): cask.Response[String] =
    cask.Response("", statusCode = 204, headers = Seq(
      "Access-Control-Allow-Origin" -> allowedOrigin,
      "Access-Control-Allow-Methods" -> "POST, OPTIONS",
      "Access-Control-Allow-Headers" -> "Content-Type, X-Player-Id"
    ))

  @cask.post("/api/rooms/:code/reveal")
  def revealOnlineCards(
                         code: String,
                         request: cask.Request
                       ): cask.Response[String] =
    Try(ujson.read(request.text())).toEither match
      case Left(_) =>
        errorResponse("Malformed JSON", 400)

      case Right(obj: ujson.Obj) =>
        val playerId = obj.value.get("playerId")
        val playerToken = obj.value.get("playerToken")

        (playerId, playerToken) match
          case (Some(ujson.Str(id)), Some(ujson.Str(token))) =>
            PokerRoomManager.revealCards(code, id, token) match
              case Left(error) =>
                errorResponse(error, 400)

              case Right(room) =>
                jsonResponse(onlineRoomJson(room, id))

          case _ =>
            errorResponse(
              "Fields 'playerId' and 'playerToken' are required",
              400
            )

      case Right(_) =>
        errorResponse("Request body must be a JSON object", 400)

  @cask.options("/api/rooms/:code/restart")
  def voteToRestartOptions(code: String): cask.Response[String] =
    cask.Response("", statusCode = 204, headers = Seq(
      "Access-Control-Allow-Origin" -> allowedOrigin,
      "Access-Control-Allow-Methods" -> "POST, OPTIONS",
      "Access-Control-Allow-Headers" -> "Content-Type, X-Player-Id"
    ))

  @cask.post("/api/rooms/:code/restart")
  def voteToRestart(code: String, request: cask.Request): cask.Response[String] =
    val body = ujson.read(request.text())
    val playerId = body("playerId").str
    val playerToken = body("playerToken").str

    PokerRoomManager.voteToRestart(code, playerId, playerToken) match
      case Left(error) =>
        errorResponse("Malformed JSON", 400)

      case Right(room) =>
        jsonResponse(onlineRoomJson(room, playerId))

  @tailrec
  private def playOpponentTurns(
                                 game: PokerGame,
                                 remainingActions: Int = 20
                               ): Either[String, PokerGame] = {

    if
      game.phase == PokerPhase.Complete ||
        game.phase == PokerPhase.Showdown ||
        game.currentPlayerId != "player2"
    then
      Right(game)

    else if remainingActions <= 0 then
      Left("Opponent AI exceeded the maximum number of actions")

    else

      val aiAction =
        PokerAi.decide(
          game,
          "player2"
        )

      PokerGame.applyAction(
        game,
        aiAction
      ) match

        case Left(error) =>
          Left(error)

        case Right(updatedGame) =>
          playOpponentTurns(
            updatedGame,
            remainingActions - 1
          )
  }


  private def gameJson(
                        game: PokerGame,
                        viewerPlayerId: String = "player1",
                        revealedPlayerIds: Set[String] = Set.empty,
                        online: Boolean = false
                      ): String =
    def cardValue(card: Card): ujson.Value =
      ujson.Obj(
        "rank" -> ujson.Str(card.rank.toString),
        "suit" -> ujson.Str(card.suit.toString)
      )

    def cardsValue(cards: List[Card]): ujson.Value =
      ujson.Arr.from(cards.map(cardValue))

    val highestBet =
      game.players
        .filterNot(_.folded)
        .map(_.currentBet)
        .maxOption
        .getOrElse(0)

    val playersValue =
      ujson.Arr.from(
        game.players.map { player =>
          val isYou = player.id == viewerPlayerId

          val canSeeCards =
            isYou ||
              (online && revealedPlayerIds.contains(player.id)) ||
              (!online &&
                game.phase == PokerPhase.Complete &&
                game.winnerPlayerIds.nonEmpty &&
                game.communityCards.length == 5 &&
                !player.folded)

          val visibleCards: List[Card] =
            if canSeeCards then player.holeCards
            else Nil

          ujson.Obj(
            "id" -> ujson.Str(player.id),
            "name" -> ujson.Str(player.name),
            "chips" -> ujson.Num(player.chips),
            "currentBet" -> ujson.Num(player.currentBet),
            "toCall" -> ujson.Num(
              math.max(0, highestBet - player.currentBet)
            ),
            "folded" -> ujson.Bool(player.folded),
            "allIn" -> ujson.Bool(player.allIn),
            "holeCards" -> cardsValue(visibleCards),
            "holeCardsCount" -> ujson.Num(player.holeCards.length)
          )
        }
      )

    val legal =
      PokerLegalActions.calculate(
        game,
        game.currentPlayerId
      )

    val legalActionsValue =
      ujson.Obj(
        "playerId" -> (
          if game.currentPlayerId.nonEmpty then
            ujson.Str(game.currentPlayerId)
          else
            ujson.Null
          ),
        "actions" -> ujson.Arr.from(
          legal.actions.map(action => ujson.Str(action))
        ),
        "toCall" -> ujson.Num(legal.toCall),
        "minBet" -> legal.minBet
          .map(value => ujson.Num(value))
          .getOrElse(ujson.Null),
        "maxBet" -> legal.maxBet
          .map(value => ujson.Num(value))
          .getOrElse(ujson.Null),
        "minRaiseTo" -> legal.minRaiseTo
          .map(value => ujson.Num(value))
          .getOrElse(ujson.Null),
        "maxRaiseTo" -> legal.maxRaiseTo
          .map(value => ujson.Num(value))
          .getOrElse(ujson.Null),
        "betIsAllInOnly" -> ujson.Bool(
          legal.betIsAllInOnly
        ),
        "raiseIsShortAllInOnly" -> ujson.Bool(
          legal.raiseIsShortAllInOnly
        )
      )

    val actionHistoryValue =
      ujson.Arr.from(
        game.actionHistory.map { record =>
          val playerName =
            game.players
              .find(_.id == record.playerId)
              .map(_.name)
              .getOrElse("Unknown player")

          val actionValue: ujson.Value =
            record.action match
              case PokerAction.Fold =>
                ujson.Obj("type" -> ujson.Str("Fold"))

              case PokerAction.Check =>
                ujson.Obj("type" -> ujson.Str("Check"))

              case PokerAction.Call =>
                ujson.Obj("type" -> ujson.Str("Call"))

              case PokerAction.Bet(amount) =>
                ujson.Obj(
                  "type" -> ujson.Str("Bet"),
                  "amount" -> ujson.Num(amount)
                )

              case PokerAction.Raise(amount) =>
                ujson.Obj(
                  "type" -> ujson.Str("Raise"),
                  "amount" -> ujson.Num(amount)
                )

          ujson.Obj(
            "playerId" -> ujson.Str(record.playerId),
            "playerName" -> ujson.Str(playerName),
            "phase" -> ujson.Str(record.phase.toString),
            "action" -> actionValue,
            "potBefore" -> ujson.Num(record.potBefore),
            "communityCards" -> cardsValue(record.communityCards)
          )
        }
      )

    ujson.write(
      ujson.Obj(
        "phase" -> ujson.Str(game.phase.toString),
        "pot" -> ujson.Num(game.pot),
        "dealerPlayerId" -> ujson.Str(game.dealerPlayerId),
        "smallBlind" -> ujson.Num(game.smallBlind),
        "bigBlind" -> ujson.Num(game.bigBlind),
        "minimumRaise" -> ujson.Num(game.minimumRaise),
        "currentPlayerId" -> ujson.Str(game.currentPlayerId),
        "winnerPlayerId" -> game.winnerPlayerId
          .map(id => ujson.Str(id))
          .getOrElse(ujson.Null),
        "winnerPlayerIds" -> ujson.Arr.from(
          game.winnerPlayerIds.map(id => ujson.Str(id))
        ),
        "winningHand" -> game.winningHand
          .map(hand => ujson.Str(hand))
          .getOrElse(ujson.Null),
        "lastAction" -> game.lastAction
          .map(action => ujson.Str(action))
          .getOrElse(ujson.Null),
        "communityCards" -> cardsValue(game.communityCards),
        "players" -> playersValue,
        "legalActions" -> legalActionsValue,
        "actionHistory" -> actionHistoryValue
      )
    )


  override def main(args: Array[String]): Unit =
    initialize()
    super.main(args)
    Thread.currentThread().join()