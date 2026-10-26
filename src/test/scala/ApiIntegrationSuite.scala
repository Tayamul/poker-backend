
import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.time.Duration
import java.net.ServerSocket
import io.undertow.Undertow

import munit.FunSuite
import ujson.*

class ApiIntegrationSuite extends FunSuite:
  private val testPort =
    val socket = new ServerSocket(0)
    try socket.getLocalPort
    finally socket.close()

  private val baseUrl = s"http://127.0.0.1:$testPort"

  private val client =
    HttpClient.newBuilder()
      .connectTimeout(Duration.ofSeconds(2))
      .build()

  private var server: Undertow = _

  override def beforeAll(): Unit =
    super.beforeAll()
    Api.resetGameStateForTests()
    Api.initializeForTests()

    server = Undertow.builder()
      .addHttpListener(testPort, "127.0.0.1")
      .setHandler(Api.defaultHandler)
      .build()

    server.start()

  override def afterAll(): Unit =
    try
      if server != null then server.stop()
    finally
      super.afterAll()

  private def request(
                       method: String,
                       path: String,
                       body: Option[String] = None
                     ): HttpResponse[String] =
    val builder =
      HttpRequest.newBuilder()
        .uri(URI.create(baseUrl + path))
        .timeout(Duration.ofSeconds(5))

    body.foreach { json =>
      builder
        .header("Content-Type", "application/json")
        .method(method, HttpRequest.BodyPublishers.ofString(json))
    }

    if body.isEmpty then
      builder.method(method, HttpRequest.BodyPublishers.noBody())

    client.send(
      builder.build(),
      HttpResponse.BodyHandlers.ofString()
    )

  private def startGame(): ujson.Value =
    val response = request("POST", "/api/game/start")
    assertEquals(response.statusCode(), 200)
    ujson.read(response.body())

  override def beforeEach(context: BeforeEach): Unit =
    super.beforeEach(context)
    Api.resetGameStateForTests()

  test("GET /api/game returns 404 before a game starts") {
    val response =
      request("GET", "/api/game")

    assertEquals(response.statusCode(), 404)

    val json = ujson.read(response.body())
    assertEquals(
      json("error").str,
      "No game has been started"
    )
  }

  test("POST /api/game/start returns an initial game") {
    val game = startGame()

    assertEquals(game("phase").str, "PreFlop")
    assertEquals(game("pot").num, 15.0)
    assert(game("players").arr.nonEmpty)
    assert(game("legalActions").obj.contains("actions"))
  }

  test("GET /api/game returns the current game") {
    startGame()

    val response =
      request("GET", "/api/game")

    assertEquals(response.statusCode(), 200)

    val game = ujson.read(response.body())

    assertEquals(game("phase").str, "PreFlop")
    assertEquals(game("pot").num, 15.0)
  }

  test("an invalid action returns 400") {
    startGame()

    val response =
      request(
        "POST",
        "/api/game/action",
        Some("""{"action":"invalid"}""")
      )

    assertEquals(response.statusCode(), 400)

    val json = ujson.read(response.body())
    assert(json.obj.contains("error"))
  }

  test("malformed JSON returns 400") {
    startGame()

    val response =
      request(
        "POST",
        "/api/game/action",
        Some("""{"action":""")
      )

    assertEquals(response.statusCode(), 400)

    val json = ujson.read(response.body())
    assert(json.obj.contains("error"))
  }

  test("POST /api/rooms/:code/restart records a player's vote") {
    val (room, hostCredentials) =
      PokerRoomManager.createRoom("Host").toOption.get

    PokerRoomManager.joinRoom(room.code, "Guest").toOption.get

    PokerRoomManager.markSessionOverForTests(room.code)

    val body = ujson.Obj(
      "playerId" -> hostCredentials.playerId,
      "playerToken" -> hostCredentials.playerToken
    ).render()

    val response = request(
      "POST",
      s"/api/rooms/${room.code}/restart",
      Some(body)
    )

    assertEquals(response.statusCode(), 200)

    val json = ujson.read(response.body()) match
      case obj: ujson.Obj => obj
      case other =>
        fail(s"Expected a JSON object but got: $other")

    assertEquals(json("sessionOver").bool, true)
    assert(
      json("restartVotes").arr
        .exists(_.str == hostCredentials.playerId)
    )
  }
