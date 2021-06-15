package de.htwg.se.mill.controller.controllerComponent.controllerBaseImpl

import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.HttpMethods.{GET, POST}
import akka.http.scaladsl.model._
import akka.http.scaladsl.unmarshalling.Unmarshal
import com.google.inject.name._
import com.google.inject.{Guice, Inject, Injector, Key}
import de.htwg.se.mill.MillModule
import de.htwg.se.mill.controller.controllerComponent._
import play.api.libs.json.{JsValue, Json}

import scala.concurrent.duration.{Duration, DurationInt}
import scala.concurrent.{Await, ExecutionContextExecutor, Future}
import scala.swing.Publisher
import scala.util.{Failure, Success}

class Controller extends ControllerInterface with Publisher {
  implicit val system: ActorSystem[Nothing] = ActorSystem(Behaviors.empty, "SingleRequest")
  implicit val executionContext: ExecutionContextExecutor = system.executionContext
  val injector: Injector = Guice.createInjector(new MillModule)
  var gameState: String = GameState.handle(NewState())
  var cachedFieldAsHtml: String = ""

  @Inject
  @Named("savefio") var saveAddr: String = _
  @Inject
  @Named("loadfio") var loadAddr: String = _

  val playerHttpServer: String = sys.env.getOrElse("PLAYERHTTPSERVER", "localhost:8081")
  val fileIOHttpServer: String = sys.env.getOrElse("FILEIOHTTPSERVER", "localhost:8082")
  val roundManagerHttpServer: String = sys.env.getOrElse("ROUNDMANAGERHTTPSERVER", "localhost:8083")

  def createPlayer(name: String, number: Int = 1): Future[String] = {
    sendRequest(s"http://$playerHttpServer/player?number=$number&name=$name", POST)
      .flatMap(_.entity.toStrict(5.seconds).map(s => s.data.utf8String))
  }

  def createEmptyField(size: Int): Unit = {
    asyncRequest(s"http://$roundManagerHttpServer/field/createField?size=$size", POST)({
      _ =>
        gameState = GameState.handle(NewState())
        publish(new FieldChanged)
    })
  }

  def createEmptyFieldSync(size: Int): String = {
    val field: String = blockRequest(s"http://$roundManagerHttpServer/field/createField?size=$size", POST)
    gameState = GameState.handle(NewState())
    publish(new FieldChanged)
    field
  }

  def createRandomField(size: Int): Unit = {
    asyncRequest(s"http://$roundManagerHttpServer/field/createRandomField?size=$size", POST)({
      _ =>
        gameState = GameState.handle(RandomState())
        publish(new FieldChanged)
    })
  }

  def createRandomFieldSync(size: Int): String = {
    val field: String = blockRequest(s"http://$roundManagerHttpServer/field/createRandomField?size=$size", POST)
    publish(new FieldChanged)
    gameState = GameState.handle(RandomState())
    field
  }

  def handleClick(row: Int, col: Int)(oncomplete: Option[String] => Unit = {
    _ =>
  }): Unit =
    asyncRequest(s"http://$roundManagerHttpServer/handleClick?row=$row&col=$col", POST)(value => {
      oncomplete(value)
      turn()
      value match {
        case Some(cell) =>
          val json: JsValue = Json.parse(cell)
          val parsedCell = ((json \ "row").get.toString.toInt, (json \ "col").get.toString.toInt)
          parsedCell match {
            case (-1, -1) => publish(new CellChanged)
            case (row, col) => publish(TwoCellsChanged(row, col))
            case _ => publish(new FieldChanged)
          }
      }
    })

  def handleClickSync(row: Int, col: Int): Unit = {
    blockRequest(s"http://$roundManagerHttpServer/handleClick?row=$row&col=$col", POST)
    turn()
    publish(new CellChanged)
  }

  def undo(): Unit = {
    asyncRequest(s"http://$roundManagerHttpServer/undo", POST)(_ => {
      turn()
      publish(new FieldChanged)
    })
    gameState = GameState.handle(UndoState())
  }

  def redo(): Unit = {
    asyncRequest(s"http://$roundManagerHttpServer/redo", POST)(_ => {
      turn()
      publish(new FieldChanged)
    })
    gameState = GameState.handle(RedoState())
  }

  def changeSaveMethod(method: String): Unit = {
    method match {
      case "db" =>
        printf(s"Changed to database saving.\n")
        saveAddr = injector.getInstance(Key.get(classOf[String], Names.named("savedb")))
        loadAddr = injector.getInstance(Key.get(classOf[String], Names.named("loaddb")))
      case _ =>
        printf(s"Changed to file saving.\n")
        saveAddr = injector.getInstance(Key.get(classOf[String], Names.named("savefio")))
        loadAddr = injector.getInstance(Key.get(classOf[String], Names.named("loadfio")))
    }
  }

  def save(): Unit = {
    asyncRequest(s"http://$roundManagerHttpServer/field/json", GET) {
      case Some(field) =>
        Http().singleRequest(HttpRequest(method = HttpMethods.POST, uri = saveAddr.format(fileIOHttpServer), entity = Json.prettyPrint(Json.parse(field))))
        gameState = GameState.handle(SaveState())
        publish(new CellChanged)
      case None =>
        print(s"Saving failed\n");
    }
  }

  def load(): Unit = {
    gameState = GameState.handle(LoadState())
    asyncRequest(s"http://$fileIOHttpServer/fileio", GET) {
      case Some(field) =>
        Http().singleRequest(HttpRequest(method = HttpMethods.POST, uri = s"http://$roundManagerHttpServer/field/setField", entity = Json.prettyPrint(Json.parse(field))))
        gameState = GameState.handle(LoadState())
        publish(new FieldChanged)
      case None =>
        print(s"Loading failed\n");
    }
  }

  def saveDB(): Unit = {
    asyncRequest(s"http://$roundManagerHttpServer/field/json", GET) {
      case Some(field) =>
        Http().singleRequest(HttpRequest(method = HttpMethods.POST, uri = s"http://$fileIOHttpServer/fileio/db", entity = Json.prettyPrint(Json.parse(field))))
        gameState = GameState.handle(SaveState())
        publish(new CellChanged)
      case None =>
        print(s"Saving failed\n");
    }
  }

  def loadDB(id: Int): Unit = {
    gameState = GameState.handle(LoadState())
    asyncRequest(s"http://$fileIOHttpServer/fileio/db?id=$id", GET) {
      case Some(field) =>
        Http().singleRequest(HttpRequest(method = HttpMethods.POST, uri = s"http://$roundManagerHttpServer/field/setField", entity = Json.prettyPrint(Json.parse(field))))
        gameState = GameState.handle(LoadState())
        publish(new FieldChanged)
      case None =>
        print(s"Loading failed\n");
    }
  }

  def isSet(row: Int, col: Int)(oncomplete: Option[String] => Unit): Unit = asyncRequest(s"http://$roundManagerHttpServer/field/isSet?row=$row&col=$col")(oncomplete)

  def color(row: Int, col: Int)(oncomplete: Option[String] => Unit): Unit = asyncRequest(s"http://$roundManagerHttpServer/field/color?row=$row&col=$col")(oncomplete)

  def possiblePosition(row: Int, col: Int): Boolean = {
    val horizontalCells = List((0, 1), (0, 2), (0, 4), (0, 5), (1, 2), (1, 4), (5, 2), (5, 4), (6, 1), (6, 2), (6, 4), (6, 5))
    val verticalCells = List((1, 0), (1, 6), (2, 0), (2, 1), (2, 5), (2, 6), (4, 0), (4, 1), (4, 5), (4, 6), (5, 0), (5, 6))
    !horizontalCells.contains((row, col)) && !verticalCells.contains((row, col))
  }

  def getMillState(oncomplete: Option[String] => Unit): Unit = asyncRequest(s"http://$roundManagerHttpServer/field/millState")(oncomplete)

  def turn(): Unit = asyncRequest(s"http://$roundManagerHttpServer/turn")({ case Some(state) => gameState = GameState.whichState(state).handle })

  def fieldsize: Int = 7

  def fieldToHtml(oncomplete: Option[String] => Unit): Unit = asyncRequest(s"http://$roundManagerHttpServer/field/html")(oncomplete)

  def fieldToHtmlSync: String = blockRequest(s"http://$roundManagerHttpServer/field/html", GET)

  def fieldToString(oncomplete: Option[String] => Unit): Unit = asyncRequest(s"http://$roundManagerHttpServer/field/string")(oncomplete)

  def fieldToJson(oncomplete: Option[String] => Unit): Unit = asyncRequest(s"http://$roundManagerHttpServer/field/json")(oncomplete)

  def getRoundCounter(oncomplete: Option[String] => Unit): Unit = asyncRequest(s"http://$roundManagerHttpServer/roundCounter")(oncomplete)

  def getWinner(oncomplete: Option[String] => Unit): Unit = asyncRequest(s"http://$roundManagerHttpServer/winner")(oncomplete)

  def getWinnerText(oncomplete: Option[String] => Unit): Unit = asyncRequest(s"http://$roundManagerHttpServer/winnerText")(oncomplete)

  private def sendRequest(uri: String, method: HttpMethod): Future[HttpResponse] = {
    implicit val system: ActorSystem[Nothing] = ActorSystem(Behaviors.empty, "SingleRequest")
    implicit val executionContext: ExecutionContextExecutor = system.executionContext
    Http().singleRequest(HttpRequest(method = method, uri = uri))
  }

  private def asyncRequest(uri: String, method: HttpMethod = HttpMethods.GET, errMsg: String = "Async Request failed.")
                          (oncomplete: Option[String] => Unit): Unit = {
    sendRequest(uri, method).onComplete({
      case Failure(_) => sys.error(errMsg)
      case Success(value) => unmarshalAsync(value)(oncomplete)
    })
  }

  private def blockRequest(uri: String, method: HttpMethod, failed: String = ""): String = {
    block(sendRequest(uri, method)) match {
      case Some(response) =>
        block(unmarshal(response)) match {
          case Some(unpackedString) => unpackedString
          case None => failed
        }
      case None => failed
    }
  }

  private def block[T](future: Future[T]): Option[T] = {
    Await.ready(future, Duration.Inf).value.get match {
      case Success(s) => Some(s)
      case Failure(_) => None
    }
  }

  private def unmarshalAsync(value: HttpResponse)(oncomplete: Option[String] => Unit): Unit = {
    unmarshal(value).onComplete {
      case Failure(_) =>
        sys.error("Unmarshalling went wrong")
        oncomplete(None)
      case Success(s) =>
        oncomplete(Some(s.stripPrefix(s"${'"'}").stripSuffix(s"${'"'}")))
    }
  }

  private def unmarshal(value: HttpResponse): Future[String] = {
    implicit val system: ActorSystem[Nothing] = ActorSystem(Behaviors.empty, "SingleRequest")
    implicit val executionContext: ExecutionContextExecutor = system.executionContext
    Unmarshal(value.entity).to[String]
  }
}