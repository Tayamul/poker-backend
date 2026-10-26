
import munit.FunSuite

class ActionRequestParserSuite extends FunSuite:

  test("rejects malformed JSON"):
    assertEquals(
      ActionRequestParser.parse("{"),
      Left("Malformed JSON")
    )

  test("rejects a request without an action"):
    assertEquals(
      ActionRequestParser.parse("{}"),
      Left("Missing required field 'action'")
    )

  test("rejects a non-string action"):
    assertEquals(
      ActionRequestParser.parse("""{"action":123}"""),
      Left("Field 'action' must be a string")
    )

  test("rejects fractional raise amounts"):
    assertEquals(
      ActionRequestParser.parse(
        """{"action":"raise","amount":1.5}"""
      ),
      Left("Amount must be a positive integer")
    )

  test("rejects zero bet amounts"):
    assertEquals(
      ActionRequestParser.parse(
        """{"action":"bet","amount":0}"""
      ),
      Left("Amount must be a positive integer")
    )

  test("rejects unknown actions"):
    assertEquals(
      ActionRequestParser.parse(
        """{"action":"all-in"}"""
      ),
      Left("Unknown action. Use fold, check, call, bet, or raise")
    )

  test("accepts fold"):
    assertEquals(
      ActionRequestParser.parse("""{"action":"fold"}"""),
      Right(PokerAction.Fold)
    )

  test("accepts a positive integer raise amount"):
    assertEquals(
      ActionRequestParser.parse(
        """{"action":"raise","amount":100}"""
      ),
      Right(PokerAction.Raise(100))
    )
