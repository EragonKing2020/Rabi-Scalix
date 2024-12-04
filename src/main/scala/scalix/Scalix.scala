package scalix

import org.json4s.*
import org.json4s.native.JsonMethods.*

import java.io.PrintWriter
import scala.util.Using

case class FullName(name: String, surname: String)

class Scalix:
    def key = "1f1a29a9202654e114671efe1078f4e7"
    def url = s"https://api.themoviedb.org/3/"
    def endUrl = s"api_key=$key"

    def getParsedDataFromActor(actor: FullName): Option[JValue] =
        try {
            Some(parse(scala.io.Source.fromURL(url + "search/person?query=" + actor.name + "%20" + actor.surname + "&" + endUrl).mkString))
        } catch {
            case e: Exception => None
        }
    
    def getJsonFromUrl(url: String): Option[String] =
        try {
            println(url)
            Some(scala.io.Source.fromURL(url).mkString)
        } catch {
            case e: Exception => None
        }

    def writeJson(fileName : String, content : String): Unit = {
        val out = new PrintWriter("./cache/" + fileName + ".json")
        out.print(content)
        out.close()
    }

    def readJson(fileName : String): Option[String] = {
        val res = Using(scala.io.Source.fromFile("./cache/" + fileName + ".json")){ res => res.mkString}
        println("read : " + res);
        return res.toOption.map(b => b.toString)
    }


    def findActorId(name: String, surname: String): Option[Int] =
        val fileName = s"findActor-$name $surname"
        val request = url + "search/person?query=" + name + "%20" + surname + "&" + endUrl
        val json : Option[String] = readJson(fileName).orElse(getJsonFromUrl(request))
        json match
            case Some(actorInfo) => {
                writeJson(fileName,actorInfo)
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

    def collaboration(actor1: FullName, actor2: FullName): Set[(String, String)] =
        (findActorId(actor1.name,actor1.surname),findActorId(actor2.name,actor2.surname)) match
            case (Some(id1),Some(id2)) => {
                val movies1 = for {
                    case (id, name) <- findActorMovies(id1)
                } yield (id, name)
                val commonMovies = for {
                    case (id, name) <- findActorMovies(id2) if movies1.contains((id, name))
                } yield (id, name)
                commonMovies.map((id,name) => name).zip(commonMovies.map((id,name) => findMovieDirector(id) match
                    case Some(directorId, directorName) => directorName
                    case None => "No director specified.")
                )
            }
            case _ => Set()


object Scalix extends App:
    val test = Scalix()
    /*println(test.readJson("test"))
    println(test.writeJson("test", "{\"json\" : \"AAAAAAAA\"}"))
    println(test.readJson("test").get)*/
    println(test.findActorId("Tom", "Cruise"))
    println(test.findActorMovies(1))
    println(test.findMovieDirector(10000))
    println(test.collaboration(FullName("Tom","Cruise"), FullName("Emily","Blunt")))
    /*val key = "1f1a29a9202654e114671efe1078f4e7"
    val url = s"https://api.themoviedb.org/3/search/person?query=Tom%20Cruise&api_key=$key"
    val source = scala.io.Source.fromURL(url)
    val contents = source.mkString
    println(contents)
    val json = parse(contents)
    println(json)*/
