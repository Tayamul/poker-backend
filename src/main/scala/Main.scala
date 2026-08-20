@main
def main(): Unit =

  val deck = new Deck()

  println(s"Number of cards: ${deck.cards.length}")

  deck.cards.foreach(println)