package scalix

import org.json4s.*
import org.json4s.native.JsonMethods.*

import java.io.{File, PrintWriter}
import scala.collection.mutable
import scala.util.Using

case class FullName(name: String, surname: String)

class Scalix:
    private val key = "1f1a29a9202654e114671efe1078f4e7"
    private val url = s"https://api.themoviedb.org/3/"
    private val endUrl = s"api_key=$key"
    private val mapCache: mutable.Map[String, String] = mutable.Map[String, String]()

    /**
     * Get the json resulting of calling the TMDB database with the url : url + middleUrl + endUrl.
     * @param middleUrl The part of the URL changing between all the request.
     * @param cache If yes or no the caches are used (Save in the caches if true).
     * @return Option[String] : The json obtained, if no error.
     */
    private def getFromUrl(middleUrl: String, cache : Boolean): Option[String] =
        try {
            println("Get from url : " + url + middleUrl + "[endUrl]")
            val json = Some(scala.io.Source.fromURL(url + middleUrl + endUrl).mkString)
            if (cache) {
                writeJson(middleUrl, json.getOrElse(""))
                writeMapCache(middleUrl, json.getOrElse(""))
            }
            json
        } catch {
            case e: Exception => None
        }

    /**
     * Get an eligible fileName from a middleUrl, without extension.
     * @param middleUrl The middleUrl.
     * @return the fileName, without extension.
     */
    private def getFileNameFromMiddleUrl(middleUrl : String): String = {
        middleUrl.replace("%20", " ").replace("&", "").replace("?","").replace("=", "").replace("/", "")
    }

    /**
     * Write a json, named using middleUrl (converted into an eligible name), with the content @content.
     * @param middleUrl The middleUrl.
     * @param content The content of the Json.
     */
    private def writeJson(middleUrl : String, content : String): Unit = {
        val dir = new File("./cache")
        if (!dir.exists) dir.mkdirs()
        val out = new PrintWriter("./cache/" + getFileNameFromMiddleUrl(middleUrl) + ".json")
        out.print(content)
        println("Write Json cache " + getFileNameFromMiddleUrl(middleUrl) + ".json")
        out.close()
    }

    /**
     * Read the Json named using middleUrl (converted into an eligible name), and return it's content.
     * @param middleUrl The middleUrl.
     * @return Option[String] : The content of the file, if existing.
     */
    private def readJson(middleUrl : String): Option[String] = {
        println("Read Json cache : " + getFileNameFromMiddleUrl(middleUrl) + ".json")
        val res = Using(scala.io.Source.fromFile("./cache/" + getFileNameFromMiddleUrl(middleUrl) + ".json")){ res => res.mkString}.toOption
        writeMapCache(middleUrl, res.getOrElse(""))
        res
    }

    /**
     * Read the Map cache using middleUrl as key.
     * @param middleUrl The middleUrl.
     * @return Option[String] : The content of the cache, if existing.
     */
    private def readMapCache(middleUrl : String) : Option[String] = {
        println("Read Map cache " + middleUrl)
        mapCache.get(middleUrl)
    }

    /**
     * Write in the Map cache, using middleUrl has key and content as value.
     * @param middleUrl The middleUrl.
     * @param content the content.
     */
    private def writeMapCache(middleUrl : String, content : String): Unit = {
        println("Write Map cache " + middleUrl)
        mapCache.update(middleUrl, content)
    }

    /**
     * Get the data related to the request corresponding to middleUrl, by looking into the Map cache and the Json cache before calling TMDB.
     * @param middleUrl The middleUrl
     * @return Option[JValue] : the data returned by the request as a JValue.
     */
    private def getData(middleUrl : String): Option[JValue] = {
        readMapCache(middleUrl).orElse(readJson(middleUrl).orElse(getFromUrl(middleUrl, true))).map(x => parse(x)) //With caches
        //getFromUrl(middleUrl, false).map(x => parse(x)); //Without any caches
    }

    /**
     * Find the id of the actor having name @name and surname @surname.
     * @param name The name of the actor.
     * @param surname The surname of the actor.
     * @return Option[Int] : The id of the actor, if found.
     */
    def findActorId(name: String, surname: String): Option[Int] =
        val middleUrl = "search/person?query=" + name + "%20" + surname + "&"
        val json = getData(middleUrl)
        json match
            case Some(actorInfo) =>
                val ids = for {
                    case JObject(o) <- actorInfo
                    case JField("id", JInt(id)) <- o
                } yield id
                ids match
                    case h::t => Some(h.toInt)
                    case Nil => None
            case None => None

    /**
     * Find the movies of an actor using his id.
     * @param actorId The id of the actor.
     * @return Set[(Int, String)] : A set with all the movies where the actor played.
     */
    def findActorMovies(actorId : Int): Set[(Int, String)] = {
        val json = getData(s"person/$actorId/movie_credits?")
        json match
            case Some(moviesInfo) =>
                val tbl = for {
                    case JObject(o) <- moviesInfo
                    case JField("original_title", JString(title)) <- o
                    case JField("id", JInt(id)) <- o
                } yield (id, title)
                tbl.map((id, title) => (id.toInt, title)).toSet
            case None => Set.empty
    }

    /**
     * Find the director of a movie using its id
     * @param movieId The id of the movie
     * @return Option[(Int, String)] : The id and the name of the director, if found
     */
    def findMovieDirector(movieId: Int): Option[(Int, String)] = {
        val json = getData(s"movie/$movieId/credits?")
        json match
            case Some(directorInfo) =>
                val tbl = for {
                    case JObject(o) <- directorInfo
                    case JField("job", JString("Director")) <- o
                    case JField("id", JInt(id)) <- o
                    case JField("name", JString(name)) <- o
                } yield (id.toInt, name)
                Option.when(tbl.nonEmpty)(tbl.head)
            case None => None
    }

    /**
     * Find couples (movie, director) in which actor1 and actor2 played together.
     * @param actor1 The first actor.
     * @param actor2 The second actor.
     * @return Set[(String, String)] : a set with the name of the movie and the name of the director, for each collaboration between actor1 and actor2
     */
    def collaboration(actor1: FullName, actor2: FullName): Set[(String, String)] =
        (findActorId(actor1.name,actor1.surname),findActorId(actor2.name,actor2.surname)) match
            case (Some(id1),Some(id2)) =>
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
            case _ => Set()


object Scalix extends App:
    val test = Scalix()
    println(test.findActorId("Tom", "Cruise"))
    println(test.findActorMovies(1))
    println(test.findMovieDirector(10000))
    println(test.collaboration(FullName("Tom","Cruise"), FullName("Emily","Blunt")))
    println(test.collaboration(FullName("Tom","Cruise"), FullName("Emily","Blunt")))
    /*val key = "1f1a29a9202654e114671efe1078f4e7"
    val url = s"https://api.themoviedb.org/3/search/person?query=Tom%20Cruise&api_key=$key"
    val source = scala.io.Source.fromURL(url)
    val contents = source.mkString
    println(contents)
    val json = parse(contents)
    println(json)*/
