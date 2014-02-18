package controllers

import play.api._
import play.api.mvc._

import json._

object Application extends Controller {

	import scalaz._
	import scalaz.concurrent.Task
	import scala.concurrent.Future

	import scalaz.stream._
  import Process._

	import scala.concurrent.ExecutionContext.Implicits.global

	def toF[T](task: Task[T]): Future[T] = {
		import scala.concurrent.Promise

    val p: Promise[T] = Promise()

    task.runAsync {
      case -\/(ex) => p.failure(ex)
      case \/-(r) => p.success(r)
    }

    p.future
  }

  def index = Action.async {

  	import play.api.libs.json.JsString
  	import play.api.data.mapping.Path
  	import Parser.Tokens._

    val x = Process.emitAll(Seq(
      SObj, Name("toto"), Value(JsString("tutu")),
      SObj, Name("toto2"), Value(JsString("tutu2")), EObj, EObj
    )) |> Json.withPath
    Future(Ok(x.toList.toString))

  	// val json = """{ "foo": 42, "bar": 23, "arr": 1 }"""
   //  val log = toF((Json.tokenise(json) |> Json.parser).runLog)
   //  log.map(s => Ok(s.toString))
  }

}