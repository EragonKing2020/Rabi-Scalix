package scalix.Scalix

import org.json4s.native.JsonMethods._

object Scalix extends App:
    val key = ""
    val url = s""
    val source = scala.io.Source.fromURL(url)
    val contents = source.mkString
    println(contents)
    val json = parse(contents)
    println(json)