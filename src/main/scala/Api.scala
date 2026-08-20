object Api extends cask.MainRoutes:

  @cask.get("/")
  def hello(): String =
    "Nepalese Poker API is running!"

  @cask.get("/api/deck")
  def deck(): String =
    val deck = new Deck()
    deck.cards.mkString("[", ",", "]")

  initialize()