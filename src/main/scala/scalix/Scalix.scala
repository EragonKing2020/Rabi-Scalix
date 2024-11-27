package scalix

import org.json4s.native.JsonMethods._

object Scalix extends App:
    val key = "1f1a29a9202654e114671efe1078f4e7"
    val url = s""
    val source = scala.io.Source.fromURL(url)
    val contents = source.mkString
    println(contents)
    val json = parse(contents)
    println(json)