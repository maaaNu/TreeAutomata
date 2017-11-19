package de.university_stuttgart.maaanu

import de.university_stuttgart.maaanu.parsers._

import scala.collection.JavaConverters._
import scala.collection.immutable.Seq
import scala.io.Source
import scala.util.Try

object Main extends App {

  args(0) match {
    case "1" => heightParser()
    case "2" => prodRulesParser()
    case "3" => deleteTraceParser()
    case "4" => prodRulesWithFreqParser()
    case "5" => lex2RulesParser()
    case _ => println("This Task is unknown!")
  }

  private def heightParser(): Unit = {
    for (line <- readTreeResource()) {
      println(HeightParser.parseAll(HeightParser.start, line).getOrElse(-1))
    }
  }

  private def prodRulesParser(): Unit = {
    val parser = new ProdRuleParser()
    for (line <- readTreeResource()) {
      parser.parseAll(parser.start, line)
    }

    for ((k, v) <- parser.prodMap.asScala) {
      for (prod <- v.asScala) println(k + " -> " + prod)
    }
  }

  private def deleteTraceParser(): Unit = {
    for (line <- readTreeResource()) {
      println(DeleteTraceParser.parseAll(DeleteTraceParser.start, line).getOrElse("FAIL"))
    }
  }

  private def prodRulesWithFreqParser(): Unit = {
    val parser = new FreqProductionParser()

    for (line <- readTreeResource()) {
      parser.parseAll(parser.start, line)
    }

    for ((k, v) <- parser.prodMap.asScala) {
      for (prod <- v.asScala) println(parser.freqMap.get((k, prod)) + " " + k + " " + prod)
    }
  }

  private def lex2RulesParser(): Unit = {
    val parser = new Lex2RulesParser()

    for (line <- readNoTraceTreeResource()) {
      parser.parseAll(parser.start, line)
    }

    for ((k, v) <- parser.rewrite_map().asScala) {
      println(k + "\t" + v.asScala.map(s => s._1 + " " + s._2).mkString(" "))
    }
  }

  private def readTreeResource(): Seq[String] = {
    readFileOrElse("/wsj_0100.tree")
  }

  private def readNoTraceTreeResource(): Seq[String] = {
    readFileOrElse("/wsj_0100.noTrace")
  }

  private def readFileOrElse(file: String): Seq[String] = {
    val resource: String = Try(args(1)) getOrElse file
    Source.fromURL(getClass.getResource(resource)).getLines().toList
  }
}

