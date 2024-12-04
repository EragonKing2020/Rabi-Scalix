package scalix

import org.json4s._
import org.json4s.native.JsonMethods._

case class FullName(name: String, surname: String)

class Scalix:
    def key = "1f1a29a9202654e114671efe1078f4e7"
    def url = s"https://api.themoviedb.org/3/"
    def endUrl = s"api_key=$key"

    def getJsonFromUrl(url: String): Option[String] =
        try {
            println(url)
            Some(scala.io.Source.fromURL(url).mkString)
        } catch {
            case e: Exception => None
        }

    def findActorId(name: String, surname: String): Option[Int] =
        val request = url + "search/person?query=" + name + "%20" + surname + "&" + endUrl
        println(request)
        getJsonFromUrl(request) match
            case Some(actorInfo) => {
                val parsedActorInfo = parse(actorInfo)
                val ids = for {
                    case JObject(o) <- parsedActorInfo
                    case JField("id", JInt(id)) <- o
                } yield id
                ids match
                    case h::t => Some(h.toInt)
                    case Nil => None
            }
            case None => {
                println("No actor found with this name.")
                None
            }

object Scalix extends App:
    val s = new Scalix
    println(s.findActorId("Tom", "Cruise"))