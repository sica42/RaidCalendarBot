package raidcalbot.bot

import scala.util.parsing.combinator._
import scala.util.{Try, Success => TrySuccess, Failure => TryFailure}
import com.fasterxml.jackson.databind.{JsonNode, ObjectMapper}
import com.fasterxml.jackson.databind.node._
import scala.collection.JavaConverters._

object JsonLuaTransformer {
  private val nodeFactory = JsonNodeFactory.instance
  private val objectMapper = new ObjectMapper()
  
  // Convert Lua table string to JSON
  def luaTableToJsonNode(luaTableString: String): Try[JsonNode] = {
      LuaToJsonConverter.luaTableToJsonNode(luaTableString)
  }  

  // Convert JSON node to Lua table string
  def jsonToLuaTable(jsonNode: JsonNode): String = {
    JsonToLuaConverter.jsonToLuaTable(jsonNode)
  }

  // Remove specified keys from a JSON node
  def removeKeys(node: JsonNode, keys: Set[String]): Unit = {
    if (node.isObject) {
      val objNode = node.asInstanceOf[ObjectNode]
      val fieldNames = objNode.fieldNames().asScala.toList
      fieldNames.foreach { key =>
        if (keys.contains(key)) {
          objNode.remove(key)
        } else {
          removeKeys(objNode.get(key), keys)
        }
      }
    } else if (node.isArray) {
      node.elements().asScala.foreach(removeKeys(_, keys))
    }
  }
}

object LuaToJsonConverter extends RegexParsers {
  
  private val nodeFactory = JsonNodeFactory.instance
  private val objectMapper = new ObjectMapper()
  
  // Internal value types for parsing
  sealed trait LuaValue
  case class LuaObject(fields: List[(Option[String], LuaValue)]) extends LuaValue
  case class LuaString(value: String) extends LuaValue
  case class LuaNumber(value: Double) extends LuaValue
  case class LuaBoolean(value: Boolean) extends LuaValue
  case object LuaNil extends LuaValue
  
  // Parser combinators for Lua table syntax
  override def skipWhitespace = true
  
  def luaTable: Parser[LuaValue] = "{" ~> repsep(luaField, ",") <~ "}" ^^ { fields =>
    LuaObject(fields)
  }
  
  def luaField: Parser[(Option[String], LuaValue)] = 
    (luaKey <~ "=") ~ luaValue ^^ { case key ~ value => (Some(key), value) } |
    luaValue ^^ { value => (None, value) }
  
  def luaKey: Parser[String] = 
    luaString ^^ { case LuaString(s) => s } |
    luaIdentifier |
    "[" ~> luaString <~ "]" ^^ { case LuaString(s) => s } |
    "[" ~> luaNumber <~ "]" ^^ { case LuaNumber(n) => n.toString }
  
  def luaIdentifier: Parser[String] = """[a-zA-Z_][a-zA-Z0-9_]*""".r
  
  def luaValue: Parser[LuaValue] = 
    luaTable |
    luaString |
    luaNumber |
    luaBoolean |
    luaNull
  
  def luaString: Parser[LuaString] = 
    "\"" ~> """([^"\\]|\\.)*""".r <~ "\"" ^^ { s => LuaString(unescapeString(s)) } |
    "'" ~> """([^'\\]|\\.)*""".r <~ "'" ^^ { s => LuaString(unescapeString(s)) }
  
  def luaNumber: Parser[LuaNumber] = 
    """-?(\d+\.?\d*|\.\d+)([eE][+-]?\d+)?""".r ^^ { s => LuaNumber(s.toDouble) }
  
  def luaBoolean: Parser[LuaBoolean] = 
    "true" ^^^ LuaBoolean(true) |
    "false" ^^^ LuaBoolean(false)
  
  def luaNull: Parser[LuaValue] = "nil" ^^^ LuaNil
  
  private def unescapeString(s: String): String = {
    s.replaceAll("""\\"""", "\"")
     .replaceAll("""\\\\""", "\\")
     .replaceAll("""\\n""", "\n")
     .replaceAll("""\\t""", "\t")
     .replaceAll("""\\r""", "\r")
  }
  
  // Convert LuaValue to Jackson JsonNode
  def luaValueToJsonNode(value: LuaValue): JsonNode = value match {
    case LuaObject(fields) =>
      // Check if this should be an array or object
      val (namedFields, unnamedFields) = fields.partition(_._1.isDefined)
      
      if (namedFields.isEmpty && unnamedFields.nonEmpty) {
        // Pure array
        val arrayNode = nodeFactory.arrayNode()
        unnamedFields.foreach { case (_, v) => 
          arrayNode.add(luaValueToJsonNode(v))
        }
        arrayNode
      } else if (unnamedFields.isEmpty && namedFields.nonEmpty) {
        // Pure object
        val objectNode = nodeFactory.objectNode()
        namedFields.collect {
          case (Some(key), v) => (key, v)
        }.foreach { case (key, v) =>
          val jsonNode: JsonNode = luaValueToJsonNode(v)         
          val _: JsonNode = objectNode.set(key, jsonNode)
          ()
        }

        objectNode
      } else {
        // Mixed - convert to object with numeric keys for unnamed fields
        val objectNode = nodeFactory.objectNode()
        namedFields.collect {
          case (Some(key), v) => (key, v)
        }.foreach { case (key, v) =>
          val jsonNode: JsonNode = luaValueToJsonNode(v)         
          val _: JsonNode = objectNode.set(key, jsonNode)
          ()
        }

        unnamedFields.zipWithIndex.foreach { case ((_, v), index) =>
          objectNode.set((index + 1).toString, luaValueToJsonNode(v))
        }
        objectNode
      }
    
    case LuaString(s) => nodeFactory.textNode(s)
    
    case LuaNumber(n) =>
      if (n.isWhole) nodeFactory.numberNode(n.toLong) else nodeFactory.numberNode(n)
    
    case LuaBoolean(b) => nodeFactory.booleanNode(b)
    
    case LuaNil => nodeFactory.nullNode()
  }
  
  // Main conversion function
  def luaTableToJsonNode(luaTableString: String): Try[JsonNode] = {
    parseAll(luaTable, luaTableString.trim) match {
      case Success(luaValue, _) => TrySuccess(luaValueToJsonNode(luaValue))
      case NoSuccess(msg, _) => TryFailure(new RuntimeException(s"Parse error: $msg"))
    }
  }
}

object JsonToLuaConverter {

  def jsonToLuaTable(jsonNode: JsonNode): String = {
    val keyMap = Map(
      "announcements" -> "a", "name" -> "n", "closingTime" -> "ct", "startTime" -> "s", "classes" -> "cl", "lastUpdated" -> "l", "entryTime" -> "e",
      "description" -> "d", "leaderName" -> "le", "channelType" -> "ch", "leaderId" -> "ld", "signUps" -> "si", "channelName" -> "cn", "effectiveName" -> "ef",
      "roleName" -> "r", "color" -> "c", "endTime" -> "et", "id" -> "i", "serverId" -> "se", "templateId" -> "t", "date" -> "da", "roles" -> "ro", "status" -> "st",
      "className" -> "cs", "specName" -> "sp", "position" -> "p", "time" -> "ti", "userId" -> "u", "title" -> "tl", "type" -> "ty", "limit" -> "li",
      "channelId" -> "ci", "specs" -> "sc", "displayTitle" -> "di", "signUpCount" -> "su", "closeTime" -> "co", 
      "reference" -> "re", "allowDuplicateReservation" -> "ad", "allowComments" -> "ac", "reservationLimit" -> "rl", "comment" -> "cm", "character" -> "ca",
      "raidItemId" -> "b", "itemId" -> "f", "srPlus" -> "sr",
      "specialization" -> "z", "advancedHrItems" -> "ah", "isHardReserved" -> "h", "characterSpecializations" -> "cz", "characterNames" -> "cb"
    )

    val valueMap = Map(
      "Tanks" -> "#1", "Arms" -> "#2", "Fury" -> "#3", "Protection" -> "#4", "Protection1" -> "#5", "Holy" -> "#6", "Holy1" -> "#7", "Retribution" -> "#8",
      "Guardian" -> "#9", "Combat" -> "#10", "Demonology" -> "#11", "Destruction" -> "#12", "Enhancement" -> "#13", "Dps" -> "#14", "Feral" -> "#15",
      "Assassination" -> "#16", "Subtlety" -> "#17", "Survival" -> "#18", "Beastmastery" -> "#19", "Arcane" -> "#20", "Fire" -> "#21", "Frost" -> "#22",
      "Affliction" -> "#23", "Marksmanship" -> "#24", "Balance" -> "#25", "Shadow" -> "#26", "Smite" -> "#27", "Elemental" -> "#28", "Ranged" -> "#29",
      "Discipline" -> "#30",  "Restoration" -> "#31", "Restoration1" -> "#32", "Healer" -> "#33", "Late" -> "#34", "Bench" -> "#35", "Tentative" -> "#36",
      "Absence" -> "#37", "Healers" -> "#38", "Melee" -> "#39", "Tank" -> "#40", "primary" -> "#41",
      "Druid" -> "#42", "DruidBalance" -> "#43", "DruidFeral" -> "#44", "DruidRestoration" -> "#45", "DruidBear" -> "#46", "Hunter" -> "#47",
      "HunterBeastMastery" -> "#48", "HunterMarksmanship" -> "#49", "HunterSurvival" -> "#50", "Mage" -> "#51", "MageArcane" -> "#52", "MageFire" -> "#53",
      "MageFrost" -> "#54", "Paladin" -> "#55", "PaladinHoly" -> "#56", "PaladinProtection" -> "#57", "PaladinRetribution" -> "#58", "Priest" -> "#59",
      "PriestDiscipline" -> "#60", "PriestHoly" -> "#61", "PriestShadow" -> "#62", "Rogue" -> "#63", "RogueSwords" -> "#64", "RogueDaggers" -> "#65",
      "RogueMaces" -> "#66",  "Shaman" -> "#67", "ShamanElemental" -> "#68", "ShamanEnchancement" -> "#69", "ShamanRestoration" -> "#70", "ShamanTank" -> "#71",
      "Warlock" -> "#72", "WarlockAffliction" -> "#73", "Demonology" -> "#74", "Destruction" -> "#75", "Warrior" -> "#76", "WarriorArms" -> "#77",
      "WarriorFury" -> "#78", "WarriorProtection" -> "#79"
    )

    convertNode(jsonNode, keyMap, valueMap)
  }
  
  private def convertNode(node: JsonNode, keyMap: Map[String, String], valueMap: Map[String, String]): String = {
    node match {
      case obj: ObjectNode =>
        val fields = obj.fields().asScala.toList
        if (fields.isEmpty) {
          "{}"
        } else {
          val fieldStrings = fields.map { entry =>
            val originalKey = entry.getKey
            val shortKey = keyMap.getOrElse(originalKey, originalKey)
            val value = entry.getValue
            val luaKey = if (isValidLuaIdentifier(shortKey)) shortKey else s"[${escapeLuaString(shortKey)}]"
            s"$luaKey=${convertNode(value, keyMap, valueMap)}"
          }
          s"{${fieldStrings.mkString(",")}}"
        }
      
      case arr: ArrayNode =>
        val elements = arr.elements().asScala.toList
        if (elements.isEmpty) {
          "{}"
        } else {
          val elementStrings = elements.map(element => convertNode(element, keyMap, valueMap))
          s"{${elementStrings.mkString(",")}}"
        }
      
      case textNode: TextNode =>
        val original = textNode.asText()
        val mapped = valueMap.getOrElse(original, original)
        escapeLuaString(mapped)
      
      case numNode: NumericNode =>
        numNode.asText()
      
      case boolNode: BooleanNode =>
        boolNode.asBoolean().toString
      
      case _: NullNode =>
        "nil"
      
      case _ =>
        node.asText() match {
          case text if text.nonEmpty => escapeLuaString(text)
          case _ => "nil"
        }
    }
  }
  
  private def isValidLuaIdentifier(str: String): Boolean = {
    // Lua identifiers must start with a letter or underscore, 
    // followed by letters, digits, or underscores
    str.matches("^[a-zA-Z_][a-zA-Z0-9_]*$") && !isLuaKeyword(str)
  }
  
  private def isLuaKeyword(str: String): Boolean = {
    val luaKeywords = Set(
      "and", "break", "do", "else", "elseif", "end", "false", "for",
      "function", "if", "in", "local", "nil", "not", "or", "repeat",
      "return", "then", "true", "until", "while"
    )
    luaKeywords.contains(str)
  }
  
  private def escapeLuaString(str: String): String = {
    val escaped = str
      .replace("\\", "\\\\")
      .replace("\"", "\\\"")
      .replace("\n", "\\n")
      .replace("\r", "\\r")
      .replace("\t", "\\t")
    
    "\"" + escaped + "\""
  }
}
