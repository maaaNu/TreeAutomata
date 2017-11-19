package de.university_stuttgart.maaanu

import de.university_stuttgart.maaanu.parsers.{DeleteTraceParser, HeightParser, ProdRuleParser}

import scala.collection.JavaConverters._
import scala.io.Source
import scala.util.Try

object Main extends App {

  args(0) match {
    case "1" => heightParser()
    case "2" => prodRulesParser()
    case "3" => deleteTraceParser()
    case "4" => prodRulesWithFreqParser()
    case _ => println("This Task is unknown!")
  }

  private def heightParser() = {
    for (line <- readResource()) {
      println(HeightParser.parseAll(HeightParser.start, line).getOrElse(-1))
    }
  }

  private def prodRulesParser() = {
    val parser = new ProdRuleParser()
    for(line <- readResource()) {
      parser.parseAll(parser.start, line)
    }

    for ((k, v) <- parser.prodMap.asScala) {
      for (prod <- v.asScala) println(k + " -> " + prod)
    }
  }

  private def deleteTraceParser() = {
    for (line <- readResource()) {
      println(DeleteTraceParser.parseAll(DeleteTraceParser.start, line).getOrElse("FAIL"))
    }
  }

  private def prodRulesWithFreqParser() = {

  }

  private def readResource() = {
    val resource : String = Try(args(1)) getOrElse  "/wsj_0100.tree"
    Source.fromURL(getClass.getResource(resource)).getLines().toList
  }
}

