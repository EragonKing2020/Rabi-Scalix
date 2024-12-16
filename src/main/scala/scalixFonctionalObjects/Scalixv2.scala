package scalixFonctionalObjects

import org.json4s.*
import org.json4s.native.JsonMethods.*
import java.io.PrintWriter
import scala.util.Using

trait Person {
  //This trait defines the basic characteristics of a person
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

//This case class represents a movie director
case class Director(id: Int, name: String, surname: String) extends Person

//This case class represents a movie
case class Movie(movieID: Int, title: String, director: Option[Director] = None) {
  import Scalixv2.{url, endURL}
  override def equals(obj: Any): Boolean = {
    obj match
      case obj: Movie =>
        obj.isInstanceOf[Movie] &&
          this.movieID == obj.movieID
      case _ => false
  }
  def findMovieDirector = {
    director match
      case None =>
        val source = scala.io.Source.fromURL(url + s"movie/$movieID/credits?" + endURL)
        val json = parse(source.mkString)
        val tbl = for {
          case JObject(o) <- json
          case JField("job", JString("Director")) <- o
          case JField("id", JInt(id)) <- o
          case JField("name", JString(name)) <- o
        } yield (id.toInt, name)
        Option.when(tbl.nonEmpty)(Director(tbl.head._1, tbl.head._2.split(" ").head, tbl.head._2.split(" ").last))
      case director => director
  }
}

//This case class represents an actor
case class Actor(id: Int, name: String, surname: String) extends Person {
  import Scalixv2.{url, endURL}
  def findActorMovies: Set[Movie] = {
    val source = scala.io.Source.fromURL(url + s"person/$id/movie_credits?" + endURL)
    val json = parse(source.mkString)
    val tbl = for {
      case JObject(o) <- json
      case JField("original_title", JString(title)) <- o
      case JField("id", JInt(id)) <- o
    } yield (id, title)
    tbl.map((id, title) => Movie(id.toInt, title)).toSet
  }
  def collaboration(otherActor: Actor): Set[Movie] =
    val commonMovies = for {
      case movie <- otherActor.findActorMovies if this.findActorMovies.contains(movie)
    } yield movie
    commonMovies.map(movie => Movie(movie.movieID, movie.title, movie.findMovieDirector))
}

trait Scalixv2:
  protected val key: String = "1f1a29a9202654e114671efe1078f4e7"
  def findActorID(name: String, surname: String): Option[Int]

object Scalixv2 extends Scalixv2:
  val url: String = s"https://api.themoviedb.org/3/"
  val endURL: String = s"api_key=$key"
  def getParsedDataFromPerson(name: String, surname: String): Option[JValue] =
    try {
      Some(parse(scala.io.Source.fromURL(url + "search/person?query=" + name + "%20" + surname + "&" + endURL).mkString))
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
  def writeJson(fileName: String, content: String): Unit = {
    val out = new PrintWriter("./cache/" + fileName + ".json")
    out.print(content)
    out.close()
  }
  def readJson(fileName: String): Option[String] = {
    val res = Using(scala.io.Source.fromFile("./cache/" + fileName + ".json")) { res => res.mkString }
    println("read : " + res);
    res.toOption.map(b => b.toString)
  }
  override def findActorID(name: String, surname: String): Option[Int] =
    val fileName = s"findActor-$name $surname"
    val request = url + "search/person?query=" + name + "%20" + surname + "&" + endURL
    val json : Option[String] = readJson(fileName).orElse(getJsonFromUrl(request))
    json match
      case Some(actorInfo) =>
        writeJson(fileName,actorInfo)
        val parsedActorInfo = parse(actorInfo)
        val ids = for {
          case JObject(o) <- parsedActorInfo
          case JField("id", JInt(id)) <- o
        } yield id
        ids match
          case h::t => Some(h.toInt)
          case Nil => None
      case None =>
        println("No actor found with this name.")
        None

object Test extends App:
  val test = Scalixv2

  val tomCruiseID = test.findActorID("Tom","Cruise")
  println(tomCruiseID)

  def createActorFromName(name: String, surname: String): Option[Actor] =
    val actorID = test.findActorID(name, surname)
    actorID match
      case Some(id) => Some(Actor(id, name, surname))
      case None => { println("There is no actor going by this name."); None }

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
