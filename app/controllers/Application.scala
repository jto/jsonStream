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
  	val json = """{ "foo": 42, "bar": 23 }"""
    val log = toF((Json.tokenise(json) |> Json.parser).runLog)
    log.map(s => Ok(s.toString))
  }

}