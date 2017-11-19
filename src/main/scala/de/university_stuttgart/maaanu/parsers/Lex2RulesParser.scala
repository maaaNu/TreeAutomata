package de.university_stuttgart.maaanu.parsers

import java.util.HashMap
import java.util.HashSet
import scala.collection.JavaConversions._

class Lex2RulesParser extends AbstractParser {

  val map: HashMap[String, HashSet[String]] = new HashMap()

  type T = String
  def func_term(r: String, t: String): String = insert(r, t)
  def func_expr(r: String, e: String): String = r
  def func_expr_wrapper(r: String, e: List[String]): String = func_expr(r, e.mkString(" "))
  def func_head(e: String): String = e

  val freqMap: HashMap[(String, String), Int] = new HashMap()
  val lex2rules = new HashMap[String, HashSet[(String, Int)]]()

  def insert(rule: String, prod: String): String = {
    if (prod.split(" ").length == 1) {
      var set = map.getOrDefault(rule, new HashSet[String]())
      freqMap.put((rule, prod), freqMap.getOrDefault((rule, prod), 0) + 1)
      set.add(prod)
      map.put(rule, set)
    }
    rule
  }

  def rewrite_map(): HashMap[String, HashSet[(String, Int)]] = {
    for ((k, v) <- freqMap) {
      var set = lex2rules.getOrElse(k._2, new HashSet[(String, Int)])
      set.add(k._1, v)
      lex2rules.put(k._2, set)
    }
    lex2rules
  }

}