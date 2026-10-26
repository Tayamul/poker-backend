import munit.FunSuite

class PotManagerSuite extends FunSuite:
  import PotManager.*
  private def c(id: String, amount: Int, folded: Boolean = false) = Contribution(id, amount, folded)

  test("equal contributions create one main pot"):
    val r = buildPots(List(c("a", 100), c("b", 100), c("c", 100))).toOption.get
    assertEquals(r.pots, List(Pot(300, List("a", "b", "c"), 100)))
    assertEquals(r.uncalledReturns, Map.empty[String, Int])

  test("unequal all-in contributions create main and side pots"):
    val r = buildPots(List(c("a", 50), c("b", 100), c("c", 100))).toOption.get
    assertEquals(r.pots, List(Pot(150, List("a", "b", "c"), 50), Pot(100, List("b", "c"), 100)))

  test("single contributor top layer is returned"):
    val r = buildPots(List(c("a", 50), c("b", 100))).toOption.get
    assertEquals(r.pots, List(Pot(100, List("a", "b"), 50)))
    assertEquals(r.uncalledReturns, Map("b" -> 50))

  test("folded player funds pot but is not eligible"):
    val r = buildPots(List(c("a", 100, true), c("b", 100), c("c", 100))).toOption.get
    assertEquals(r.pots, List(Pot(300, List("b", "c"), 100)))

  test("dead money from folded-only layer rolls into first eligible pot"):
    val r = buildPots(List(c("a", 100, true), c("b", 100, true), c("c", 50), c("d", 50))).toOption.get
    assertEquals(r.pots, List(Pot(300, List("c", "d"), 50)))

  test("side-pot payout is eligible-only and conserves chips"):
    val players = List(c("short", 50), c("middle", 100), c("deep", 150))
    val s = settle(players, Map("short" -> 3, "middle" -> 2, "deep" -> 1),
      List("short", "middle", "deep"), Ordering.Int).toOption.get
    assertEquals(s.payouts, Map("short" -> 150, "middle" -> 100, "deep" -> 50))
    assertEquals(s.totalPaid, players.map(_.totalContribution).sum)

  test("ties split with odd chip following supplied order"):
    val s = settle(List(c("a", 5), c("b", 5), c("c", 5)), Map("a" -> 10, "b" -> 10, "c" -> 1),
      List("b", "a", "c"), Ordering.Int).toOption.get
    assertEquals(s.payouts, Map("a" -> 7, "b" -> 8))

  test("rejects duplicate IDs and negative contributions"):
    assert(buildPots(List(c("a", 10), c("a", 10))).isLeft)
    assert(buildPots(List(c("a", -1))).isLeft)
