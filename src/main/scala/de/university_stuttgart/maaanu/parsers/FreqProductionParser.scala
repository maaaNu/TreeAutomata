package de.university_stuttgart.maaanu.parsers

import java.util.{HashMap, HashSet}

class FreqProductionParser extends ProdRuleParser {
  val freqMap: HashMap[(String, String), Int] = new HashMap()
  override def func_term(r: String, t:String): T = r

  override def insert(rule : String, prod : String) : String = {
    var set = prodMap.getOrDefault(rule, new HashSet[String]())
    freqMap.put((rule, prod), freqMap.getOrDefault((rule, prod), 0) + 1)
    set.add(prod)
    prodMap.put(rule, set)
    rule
  }
  
}