package scalixFonctionalObjects

import org.json4s.*
import org.json4s.native.JsonMethods.*
import java.io.{PrintWriter, File}
import scala.collection.mutable
import scala.util.Using

/**
 * Represents a person with basic characteristics
 */
trait Person {
  def id: Int
  def name: String
  def surname: String

  override def equals(obj: Any): Boolean = {
    obj match {
      case obj: Person =>
        obj.isInstanceOf[Person] &&
          this.id == obj.id
      case _ => false
    }
  }
}

/**
 * Defines the necessary elements to make requests to TMDB
 */
trait Data:
  protected val key: String = "1f1a29a9202654e114671efe1078f4e7"
  /**
   * Get the data related to the request corresponding to middleUrl, by looking into the Map cache and the Json cache before calling TMDB.
   *
   * @param middleUrl The middleUrl
   * @return Option[JValue] : the data returned by the request as a JValue.
   */
  def getData(middleUrl: String): Option[JValue]

object Data extends Data:
  private val url: String = s"https://api.themoviedb.org/3/"
  private val endUrl: String = s"api_key=$key"
  private val mapCache: mutable.Map[String, String] = mutable.Map[String, String]()

  /**
   * Get the json resulting of calling the TMDB database with the url : url + middleUrl + endUrl.
   *
   * @param middleUrl The part of the URL changing between all the request.
   * @param cache     If yes or no the caches are used (Save in the caches if true).
   * @return Option[String] : The json obtained, if no error.
   */
  private def getFromUrl(middleUrl: String, cache: Boolean): Option[String] =
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
   *
   * @param middleUrl The middleUrl.
   * @return the fileName, without extension.
   */
  private def getFileNameFromMiddleUrl(middleUrl: String): String = {
    middleUrl.replace("%20", " ").replace("&", "").replace("?", "").replace("=", "").replace("/", "")
  }

  /**
   * Write a json, named using middleUrl (converted into an eligible name), with the content @content.
   *
   * @param middleUrl The middleUrl.
   * @param content   The content of the Json.
   */
  private def writeJson(middleUrl: String, content: String): Unit = {
    val dir = new File("./cache")
    if (!dir.exists) dir.mkdirs()
    val out = new PrintWriter("./cache/" + getFileNameFromMiddleUrl(middleUrl) + ".json")
    out.print(content)
    println("Write Json cache " + getFileNameFromMiddleUrl(middleUrl) + ".json")
    out.close()
  }

  /**
   * Read the Json named using middleUrl (converted into an eligible name), and return it's content.
   *
   * @param middleUrl The middleUrl.
   * @return Option[String] : The content of the file, if existing.
   */
  private def readJson(middleUrl: String): Option[String] = {
    println("Read Json cache : " + getFileNameFromMiddleUrl(middleUrl) + ".json")
    val res = Using(scala.io.Source.fromFile("./cache/" + getFileNameFromMiddleUrl(middleUrl) + ".json")) { res => res.mkString }.toOption
    writeMapCache(middleUrl, res.getOrElse(""))
    res
  }

  /**
   * Read the Map cache using middleUrl as key.
   *
   * @param middleUrl The middleUrl.
   * @return Option[String] : The content of the cache, if existing.
   */
  private def readMapCache(middleUrl: String): Option[String] = {
    println("Read Map cache " + middleUrl)
    mapCache.get(middleUrl)
  }

  /**
   * Write in the Map cache, using middleUrl has key and content as value.
   *
   * @param middleUrl The middleUrl.
   * @param content   the content.
   */
  private def writeMapCache(middleUrl: String, content: String): Unit = {
    println("Write Map cache " + middleUrl)
    mapCache.update(middleUrl, content)
  }

  def getData(middleUrl: String): Option[JValue] = {
    readMapCache(middleUrl).orElse(readJson(middleUrl).orElse(getFromUrl(middleUrl, true))).map(x => parse(x)) //With caches
    //getFromUrl(middleUrl, false).map(x => parse(x)); //Without any caches
  }

/**
 * Represents a director
 *
 * @param id the id of the director
 * @param name the name of the director
 * @param surname the surname of the director
 */
case class Director(id: Int, name: String, surname: String) extends Person

/**
 * Represents a movie
 *
 * @param movieID the id of the movie
 * @param title the title of the movie
 * @param director the director of the movie, it is optional
 */
case class Movie(movieID: Int, title: String, director: Option[Director] = None) {
  import Scalixv2.data
  override def equals(obj: Any): Boolean = {
    obj match
      case obj: Movie =>
        obj.isInstanceOf[Movie] &&
          this.movieID == obj.movieID
      case _ => false
  }

  /**
   * Finds if existing the director associated to the movie
   *
   * @return the director of the movie if there is one
   */
  def findMovieDirector: Option[Director] = {
    director match
      case None =>
        val json = data.getData(s"movie/$movieID/credits?")
        json match
          case Some(info) =>
            val tbl = for {
              case JObject(o) <- info
              case JField("job", JString("Director")) <- o
              case JField("id", JInt(id)) <- o
              case JField("name", JString(name)) <- o
            } yield (id.toInt, name)
            Option.when(tbl.nonEmpty)(Director(tbl.head._1, tbl.head._2.split(" ").head, tbl.head._2.split(" ").last))
          case None => None
      case director => director
  }
}

/**
 * Represents an actor
 *
 * @param id the id of the actor
 * @param name the name of the actor
 * @param surname the surname of the actor
 */
case class Actor(id: Int, name: String, surname: String) extends Person {
  //we import tools from data to make requests to TMDB
  import Scalixv2.data

  /**
   * Finds all the movies in which the actor took part
   *
   * @return a set of movies
   */
  def findActorMovies: Set[Movie] = {
    val json = data.getData(s"person/$id/movie_credits?")
    json match
      case Some(info) =>
        val tbl = for {
          case JObject(o) <- info
          case JField("original_title", JString(title)) <- o
          case JField("id", JInt(id)) <- o
        } yield (id, title)
        tbl.map((id, title) => Movie(id.toInt, title)).toSet
      case None => Set()
  }

  /**
   * Finds all the movies featuring the actor and another actor specified
   *
   * @param otherActor the actor who collaborated
   * @return a set of movies in which both actors played
   */
  def collaboration(otherActor: Actor): Set[Movie] =
    val commonMovies = for {
      case movie <- otherActor.findActorMovies if this.findActorMovies.contains(movie)
    } yield movie
    commonMovies.map(movie => Movie(movie.movieID, movie.title, movie.findMovieDirector))
}

/**
 * Defines the elements of the application
 */
trait Scalixv2:
  //an instance of the object data to make requests to TMDB
  val data: Data = Data
  /**
   * Find the id of the actor having name @name and surname @surname.
   *
   * @param name    The name of the actor.
   * @param surname The surname of the actor.
   * @return Option[Int] : The id of the actor, if found.
   */
  def findActorID(name: String, surname: String): Option[Int]

/**
 * Implements the application
 */
object Scalixv2 extends Scalixv2:
  override def findActorID(name: String, surname: String): Option[Int] =
    val middleUrl = "search/person?query=" + name + "%20" + surname + "&"
    val json = data.getData(middleUrl)
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
 * Tests for the application
 */
object Test extends App:
  val test = Scalixv2

  val tomCruiseID = test.findActorID("Tom","Cruise")
  println(tomCruiseID)

  def createActorFromName(name: String, surname: String): Option[Actor] =
    val actorID = test.findActorID(name, surname)
    actorID match
      case Some(id) => Some(Actor(id, name, surname))
      case None => println("There is no actor going by this name."); None

  val tomCruise = createActorFromName("Tom", "Cruise")
  val actorMovies = tomCruise match
    case Some(actor) => println(actor.name)
      actor.findActorMovies
    case None => Set()
  println(actorMovies)

  val firstDirector = actorMovies.head.findMovieDirector
  println(firstDirector)

  val emilyBlunt = createActorFromName("Emily", "Blunt")
  val moviesTogether = (tomCruise, emilyBlunt) match
    case (Some(tom),Some(emily)) => tom.collaboration(emily)
    case _ => None
  println(moviesTogether)
