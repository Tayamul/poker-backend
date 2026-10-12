import scala.annotation.tailrec

object Api extends cask.MainRoutes:

  private var currentGame: Option[PokerGame] = None


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
          "http://localhost:5173"
      )
    )

  @cask.options("/api/game/action")
  def gameActionOptions(): cask.Response[String] =
    cask.Response(
      "",
      statusCode = 204,
      headers = Seq(
        "Access-Control-Allow-Origin" -> "http://localhost:5173",
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
              "http://localhost:5173"
          )
        )


  @cask.post("/api/game/action")
  def gameAction(
                  request: cask.Request
                ): cask.Response[String] =

    val body = ujson.read(request.text())

    val action =
      body("action").str

    val amount =
      body.obj
        .get("amount")
        .map(_.num.toInt)

    currentGame match

      case None =>
        cask.Response(
          """{"error":"No game has been started"}""",
          statusCode = 404,
          headers = Seq(
            "Content-Type" -> "application/json",
            "Access-Control-Allow-Origin" ->
              "http://localhost:5173"
          )
        )

      case Some(game) =>

        val pokerAction =
          parseAction(action, amount)

        pokerAction match

          case Left(error) =>
            cask.Response(
              s"""{"error":"$error"}""",
              statusCode = 400,
              headers = Seq(
                "Content-Type" -> "application/json",
                "Access-Control-Allow-Origin" ->
                  "http://localhost:5173"
              )
            )

          case Right(parsedAction) =>

            PokerGame.applyAction(
              game,
              parsedAction
            ) match

              case Left(error) =>
                cask.Response(
                  s"""{"error":"$error"}""",
                  statusCode = 400,
                  headers = Seq(
                    "Content-Type" -> "application/json",
                    "Access-Control-Allow-Origin" ->
                      "http://localhost:5173"
                  )
                )

              case Right(updatedGame) =>

                playOpponentTurns(updatedGame) match

                  case Left(error) =>
                    cask.Response(
                      s"""{"error":"$error"}""",
                      statusCode = 400,
                      headers = Seq(
                        "Content-Type" -> "application/json",
                        "Access-Control-Allow-Origin" ->
                          "http://localhost:5173"
                      )
                    )

                  case Right(finalGame) =>
                    currentGame =
                      Some(finalGame)

                    cask.Response(
                      gameJson(finalGame),
                      headers = Seq(
                        "Content-Type" -> "application/json",
                        "Access-Control-Allow-Origin" ->
                          "http://localhost:5173"
                      )
                    )

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

  private def parseAction(
                           action: String,
                           amount: Option[Int]
                         ): Either[String, PokerAction] =

    action.toLowerCase match

      case "fold" =>
        Right(PokerAction.Fold)

      case "check" =>
        Right(PokerAction.Check)

      case "call" =>
        Right(PokerAction.Call)

      case "bet" =>
        amount match
          case Some(value) if value > 0 =>
            Right(PokerAction.Bet(value))

          case _ =>
            Left("Bet requires a positive amount")

      case "raise" =>
        amount match
          case Some(value) if value > 0 =>
            Right(PokerAction.Raise(value))

          case _ =>
            Left("Raise requires a positive amount")

      case _ =>
        Left(
          "Unknown action. Use fold, check, call, bet, or raise"
        )


  private def gameJson(
                        game: PokerGame
                      ): String =

    val highestBet =
      game.players
        .filter(!_.folded)
        .map(_.currentBet)
        .maxOption
        .getOrElse(0)

    val playersJson =
      game.players
        .map { player =>

          val isYou =
            player.id == "player1"

          val canSeeCards =
            isYou ||
              game.phase == PokerPhase.Complete &&
                game.winnerPlayerIds.nonEmpty &&
                game.communityCards.length == 5 &&
                !player.folded

          val visibleCards =
            if canSeeCards then
              cardsJson(player.holeCards)
            else
              "[]"

          val toCall =
            math.max(
              0,
              highestBet - player.currentBet
            )

          s"""
          {
            "id":"${player.id}",
            "name":"${player.name}",
            "chips":${player.chips},
            "currentBet":${player.currentBet},
            "toCall":$toCall,
            "folded":${player.folded},
            "allIn":${player.allIn},
            "holeCards":$visibleCards,
            "holeCardsCount":${player.holeCards.length}
          }
          """
        }
        .mkString("[", ",", "]")

    val winnerJson =
      game.winnerPlayerId match
        case Some(id) =>
          s""""$id""""
        case None =>
          "null"

    val lastActionJson =
      game.lastAction match
        case Some(action) =>
          s""""${escapeJson(action)}""""
        case None =>
          "null"

    val winningHandJson =
      game.winningHand match
        case Some(hand) =>
          s""""${escapeJson(hand)}""""
        case None =>
          "null"

    val winnerPlayerIdsJson =
      game.winnerPlayerIds
        .map(id => s""""${escapeJson(id)}"""")
        .mkString("[", ",", "]")

    s"""
    {
      "phase":"${game.phase}",
      "pot":${game.pot},
      "dealerPlayerId":"${game.dealerPlayerId}",
      "smallBlind":${game.smallBlind},
      "bigBlind":${game.bigBlind},
      "minimumRaise":${game.minimumRaise},
      "currentPlayerId":"${game.currentPlayerId}",
      "winnerPlayerId":$winnerJson,
      "winnerPlayerIds":$winnerPlayerIdsJson,
      "winningHand":$winningHandJson,
      "lastAction":$lastActionJson,
      "communityCards":${cardsJson(game.communityCards)},
      "players":$playersJson
    }
    """


  private def escapeJson(value: String): String =
    value
      .replace("\\", "\\\\")
      .replace("\"", "\\\"")


  override def main(args: Array[String]): Unit =
    initialize()
    super.main(args)
    Thread.currentThread().join()