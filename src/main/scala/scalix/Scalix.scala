package scalix

import org.json4s.native.JsonMethods._

class Scalix extends App:
    def key = "1f1a29a9202654e114671efe1078f4e7"
    def url = s"https://api.themoviedb.org/3/search/"
    def endUrl = s"&api_key=$key"
    def findActorId(name: String, surname: String): Option[Int] = Some(3)

object Scalix:
    val key = "1f1a29a9202654e114671efe1078f4e7"
    val url = s"https://api.themoviedb.org/3/search/person?query=Tom%20Cruise&api_key=$key"
    val source = scala.io.Source.fromURL(url)
    val contents = source.mkString
    println(contents)
    //val json = parse(contents)
    //println(json)