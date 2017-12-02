package de.university_stuttgart.maaanu.parsers

import java.util.{HashMap, HashSet}


class ProdRuleParser extends AbstractParser{
  val prodMap: HashMap[String, HashSet[String]] = new HashMap()

  type T = String
  def func_term(r: String, t:String): String = insert(r, t)
  def func_expr(r: String, e: String): String = insert(r, e)
  def func_expr_wrapper(r: String, e: List[String]): String = func_expr(r, e.mkString(" "))
  def func_head(e: String): String = e

  def insert(rule : String, prod : String) : String = {
    var set = prodMap.getOrDefault(rule, new HashSet[String]())
    set.add(prod)
    prodMap.put(rule, set)
    rule
  }
  
}