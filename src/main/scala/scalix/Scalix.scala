package scalix

import org.json4s.*
import org.json4s.native.JsonMethods.*

import java.io.{File, PrintWriter}
import scala.collection.mutable
import scala.util.Using

case class FullName(name: String, surname: String)

class Scalix:
    val key = "1f1a29a9202654e114671efe1078f4e7"
    val url = s"https://api.themoviedb.org/3/"
    val endUrl = s"api_key=$key"
    val mapCache: mutable.Map[String, String] = mutable.Map[String, String]()

    def getJsonFromUrl(middleUrl: String): Option[String] =
        try {
            println("Get from url : " + url + middleUrl + "[endUrl]")
            val json = Some(scala.io.Source.fromURL(url + middleUrl + endUrl).mkString)
            writeJson(middleUrl, json.getOrElse(""))
            writeMapCache(middleUrl, json.getOrElse(""))
            json
        } catch {
            case e: Exception => None
        }

    def getFileNameFromMiddleUrl(middleUrl : String): String = {
        return middleUrl.replace("%20", " ").replace("&", "").replace("?","").replace("=", "").replace("/", "")
    }

    def writeJson(middleUrl : String, content : String): Unit = {
        val dir = new File("./cache")
        if (!dir.exists) dir.mkdirs();
        val out = new PrintWriter("./cache/" + getFileNameFromMiddleUrl(middleUrl) + ".json")
        out.print(content)
        println("Write Json cache " + getFileNameFromMiddleUrl(middleUrl) + ".json")
        out.close()
    }

    def readJson(middleUrl : String): Option[String] = {
        println("Read Json cache : " + getFileNameFromMiddleUrl(middleUrl) + ".json")
        val res = Using(scala.io.Source.fromFile("./cache/" + getFileNameFromMiddleUrl(middleUrl) + ".json")){ res => res.mkString}.toOption.map(b => b.toString)
        writeMapCache(middleUrl, res.getOrElse(""))
        return res
    }

    def readMapCache(middleUrl : String) : Option[String] = {
        println("Read Map cache " + middleUrl)
        return mapCache.get(middleUrl)
    }

    def writeMapCache(middleUrl : String, content : String): Unit = {
        println("Write Map cache " + middleUrl)
        mapCache.update(middleUrl, content)
    }

    def getData(middleUrl : String): Option[JValue] = {
        return readMapCache(middleUrl).orElse(readJson(middleUrl).orElse(getJsonFromUrl(middleUrl))).map(x => parse(x))
    }

    def findActorId(name: String, surname: String): Option[Int] =
        val middleUrl = "search/person?query=" + name + "%20" + surname + "&"
        val json = getData(middleUrl)
        json match
            case Some(actorInfo) => {
                val ids = for {
                    case JObject(o) <- actorInfo
                    case JField("id", JInt(id)) <- o
                } yield id
                ids match
                    case h::t => Some(h.toInt)
                    case Nil => None
            }
            case None => {
                None
            }

    def findActorMovies(actorId : Int): Set[(Int, String)] = {
        val json = getData(s"person/$actorId/movie_credits?")
        json match
            case Some(moviesInfo) => {
                val tbl = for {
                    case JObject(o) <- moviesInfo
                    case JField("original_title", JString(title)) <- o
                    case JField("id", JInt(id)) <- o
                } yield (id, title)
                return tbl.map((id, title) => (id.toInt, title)).toSet
            }
            case None => {
                Set.empty
            }
    }

    def findMovieDirector(movieId: Int): Option[(Int, String)] = {
        val json = getData(s"movie/$movieId/credits?")
        json match
            case Some(directorInfo) => {
                val tbl = for {
                    case JObject(o) <- directorInfo
                    case JField("job", JString("Director")) <- o
                    case JField("id", JInt(id)) <- o
                    case JField("name", JString(name)) <- o
                } yield (id.toInt, name)
                return Option.when(tbl.nonEmpty)(tbl.head)
            }
            case None => {
                None
            }
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
    //println(test.findActorId("Tom", "Cruise"))
    //println(test.findActorMovies(1))
    //println(test.findMovieDirector(10000))
    println(test.collaboration(FullName("Tom","Cruise"), FullName("Emily","Blunt")))
    println(test.collaboration(FullName("Tom","Cruise"), FullName("Emily","Blunt")))
    /*val key = "1f1a29a9202654e114671efe1078f4e7"
    val url = s"https://api.themoviedb.org/3/search/person?query=Tom%20Cruise&api_key=$key"
    val source = scala.io.Source.fromURL(url)
    val contents = source.mkString
    println(contents)
    val json = parse(contents)
    println(json)*/
