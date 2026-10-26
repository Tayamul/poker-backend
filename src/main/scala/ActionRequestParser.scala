
import scala.util.Try

object ActionRequestParser:

  def parse(bodyText: String): Either[String, PokerAction] =
    Try(ujson.read(bodyText)).toEither
      .left.map(_ => "Malformed JSON")
      .flatMap {
        case obj: ujson.Obj =>
          obj.value.get("action") match
            case Some(ujson.Str(action)) =>
              val amountResult: Either[String, Option[Int]] =
                obj.value.get("amount") match
                  case None =>
                    Right(None)

                  case Some(ujson.Num(value))
                    if value > 0 &&
                      value <= Int.MaxValue &&
                      value == value.toLong.toDouble =>
                    Right(Some(value.toInt))

                  case Some(_) =>
                    Left("Amount must be a positive integer")

              amountResult.flatMap(amount =>
                parseAction(action, amount)
              )

            case Some(_) =>
              Left("Field 'action' must be a string")

            case None =>
              Left("Missing required field 'action'")

        case _ =>
          Left("Request body must be a JSON object")
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
        Left("Unknown action. Use fold, check, call, bet, or raise")
