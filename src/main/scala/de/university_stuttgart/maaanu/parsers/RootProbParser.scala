package de.university_stuttgart.maaanu.parsers

import java.util.HashMap

import scala.collection.JavaConversions._

class RootProbParser extends AbstractParser {
   type T = String
   
  def func_term(r: String, t:String): T = r
  def func_expr(r: String, e:T): T = r
  def func_expr_wrapper(r: String, e: List[T]): T = r
  var n: Int = 0
  val map = new HashMap[String, Double]()
   
   def func_head(e: T): T = {
     n += 1
     val occ = map.getOrDefault(e, 0)
     map.put(e, occ + 1)
     return e
   }
   
   def overwriteMap() = {
     for(k <- map.keySet()) {
       map.put(k, map.get(k) / n )
     }
     map
   }
}