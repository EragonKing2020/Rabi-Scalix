package scalix

import org.json4s._
import org.json4s.native.JsonMethods._

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

    def findActorMovies(actorId : Int): Set[(Int, String)] = {
        val source = scala.io.Source.fromURL(url + s"person/$actorId/movie_credits?" + endUrl)
        val json = parse(source.mkString)
        val tbl = for {
            case JObject(o) <- json
            case JField("original_title", JString(title)) <- o
            case JField("id", JInt(id)) <- o
        } yield (id, title)
        return tbl.map((id, title) => (id.toInt ,title)).toSet
    }

    def findMovieDirector(movieId: Int): Option[(Int, String)] = {
        val source = scala.io.Source.fromURL(url + s"movie/$movieId/credits?" + endUrl)
        val json = parse(source.mkString)
        val tbl = for {
            case JObject(o) <- json
            case JField("job", JString("Director")) <- o
            case JField("id", JInt(id)) <- o
            case JField("name", JString(name)) <- o
        } yield (id.toInt, name)
        return Option.when(tbl.nonEmpty)(tbl.head)
    }

object Scalix extends App:
    val test = Scalix()
    println(test.findActorId("Tom", "Cruise"))
    println(test.findActorMovies(1))
    println(test.findMovieDirector(10000))
    /*val key = "1f1a29a9202654e114671efe1078f4e7"
    val url = s"https://api.themoviedb.org/3/search/person?query=Tom%20Cruise&api_key=$key"
    val source = scala.io.Source.fromURL(url)
    val contents = source.mkString
    println(contents)*/
    //val json = parse(contents)
    //println(json) 
