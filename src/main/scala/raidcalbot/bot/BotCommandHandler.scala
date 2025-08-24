package raidcalbot.bot

import com.typesafe.scalalogging.StrictLogging
import raidcalbot.common.Global
import raidcalbot.common.ReminderCheck
import raidcalbot.game.GamePackets
import raidcalbot.bot.JsonLuaTransformer._

import com.fasterxml.jackson.databind.{JsonNode, ObjectMapper}
import com.fasterxml.jackson.databind.node.{ObjectNode, ArrayNode}
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.fasterxml.jackson.annotation.JsonInclude

import sttp.client3._
import sttp.client3.okhttp.OkHttpSyncBackend
import sttp.model.headers.CookieWithMeta

import java.time.Instant
import java.nio.file.{Paths, Files}
import java.nio.charset.StandardCharsets
import java.io.PrintWriter
import scala.io.Source
import scala.collection.JavaConverters._
import java.util.concurrent.{Executors, TimeUnit, ScheduledExecutorService}
import scala.util.{Try, Success, Failure}
import scala.util.matching.Regex
import scala.util.Random
import scala.reflect.ClassTag

case class DiscordChannelCheckData( userId: String, channelId: String, renew: Boolean = false )
case class SRAddData(raidId: Option[Long] = None, reference: String, characterName: String, characterClass: String, specialization: String, raidItemIds: List[Int])
case class SignupData(eventId: String, userId: String, className: String, specName: String, channelId: String, name: Option[String])
case class EditSignupData(eventId: String, signupId: String, className: String, specName: String, channelId: String, name: Option[String])
case class SignupResult(status: String, success: Boolean, player: String, eventId: Option[String] = None, lastUpdated: Option[String] = None, signUp: Option[JsonNode] = None) {
  override def toString: String = {
    val jsonNode = BotCommandHandler.mapper.valueToTree[JsonNode](this)
    val luaData = jsonToLuaTable(jsonNode)

    s"SIGNUP_RESULT::$luaData"
  }
}

case class Currency(gold: Int, silver: Int, copper: Int) {
  override def toString: String = f"|cffffffff${gold}|cffffd700g|r|cffffffff${silver}|cffc7c7cfs|r|cffffffff${copper}|cffeda55fc|r"
}

object BotCommandHandler extends GamePackets with StrictLogging {
  private val reconnectExecutor = Executors.newSingleThreadScheduledExecutor
  private val backend = OkHttpSyncBackend()

  val mapper = new ObjectMapper()
    .registerModule(DefaultScalaModule)
    .setSerializationInclusion(JsonInclude.Include.NON_ABSENT)

  ReminderScheduler.scheduleReminder(Global.config.wow.hydrationCheck)
  ReminderScheduler.scheduleReminder(Global.config.wow.postureCheck)

  def apply(player: String, message: String): Unit = {
    message.split("::", 2) match {
      case Array(command, data) =>
        logger.info(s"Received $command from $player: $data")

        luaTableToJsonNode(if (data.trim.isEmpty) "{}" else data) match {
          case Success(jsonNode) =>
            command match {
              case "RBSTATUS" => handleBotStatus(player)
              case "CHCHECK" => handleChannelCheck(player, jsonNode)
              case "RDID" => handleDiscordIdLookup(player, jsonNode)
              case "RDAUTH" => handleDiscordAuthentication( player, jsonNode)
              case "RSR" => handleGetSR(player, jsonNode)
              case "SRDELETE" => handleDeleteSR(player, jsonNode)
              case "SRADD" => handleAddSR(player, jsonNode)
              case "REVENT" => handleRequestEvent(player, jsonNode)
              case "REVENTS" => handleRequestEvents(player)
              case "SIGNUP" => handleSignup(player, jsonNode)
              case "SIGNUP_EDIT" => handleSignupEdit(player, jsonNode)
              case "VER" => // Ignore version responses
              case "VERC" => // Ignore version checks
              case _ => logger.debug(s"Unknown command: $command")
            }
          case Failure(error) =>
            logger.debug(s"Command parsing error: $error")
        }
      case _ =>
        // Ignore if no command is found
    }
  }

  def whisper(name: String, message: String): Unit = {
    logger.info(s"Whisper from $name: $message");
    val commandPattern = "#([^#]+)#(.*)".r

    message match {
      case commandPattern(cmd, argsString) =>
        val command = cmd.trim.toLowerCase
        val args = if (argsString.trim.nonEmpty) argsString.trim.split("\\s+").filter(_.nonEmpty).toList else List()

        val numberPattern = "^-?\\d+$".r
        def isNumeric(s: String): Boolean = s.matches(numberPattern.regex)

        command match {
          case "pc" =>
            val response = if (args.nonEmpty) priceCheck( args.mkString(" ") ) else "No item specified"
            Global.game.foreach(_.sendMessageToWow(ChatEvents.CHAT_MSG_WHISPER, response, Some(name)))
          case "pcg" =>
            val response = if (args.nonEmpty) priceCheck( args.mkString(" ") ) else "No item specified"
            Global.game.foreach(_.sendNotification(response))
          case "gjoke" =>
            getJoke(Some("GUILD"))
          case _ =>
            Global.game.foreach(_.sendMessageToWow(ChatEvents.CHAT_MSG_WHISPER, "I have no idea what you want", Some(name)))
        }
      case _ =>
        getJoke(Some(name))
    }
  }

  /**
   * Handle Bot Status
   */
  private def handleBotStatus(player: String): Unit = {
    val discordBot = Global.discord.getBotName
    val botName = Global.config.wow.character
    sendDataToWow(CommandResponse("BSTATUS", true, player, None, Map("discordBot" -> discordBot, "botName" -> botName)))
  }

  /**
   * Handle Discord channel access check
   */
  private def handleChannelCheck(player: String, jsonNode: JsonNode): Unit = {
    JsonParsingUtils.parseJsonNode[DiscordChannelCheckData](jsonNode) match {
      case Right(data) =>
        val success =  Global.discord.hasChannelAccess(player, data.channelId, data.renew)
        sendDataToWow(CommandResponse("CHCHECK_RESULT", success, player, None, Map("channelId" -> data.channelId)))        
      case Left(errorMessage) =>
        sendDataToWow(CommandResponse("CHCHECK_RESULT", false, player, Some("Oh no, the pet did something wrong again")))
        logger.debug(s"CHCHECK error: $errorMessage")
    }
  }

  /**
   * Handle Discord ID lookup
   */
  private def handleDiscordIdLookup(player: String, jsonNode: JsonNode): Unit = {
    Option(jsonNode.get("name")).filter(!_.isNull).foreach { idNode =>
      val maybeUserId = Global.discord.getUserId(idNode.asText())
      val success = maybeUserId.isDefined
      val userId  = maybeUserId.getOrElse("")
      sendDataToWow(CommandResponse("DID", success, player, None, Map("userId" -> userId)))
    }
  }
             
  /**
   * Handle Discord authorize user
   */
  private def handleDiscordAuthentication(player: String, jsonNode: JsonNode): Unit = {
    Option(jsonNode.get("userId")).filter(!_.isNull).foreach { idNode =>
      val userId = idNode.asText()
      Global.discord.authorizeUser(userId, player)
    }
  }
                
  /**
   * Handle Request SR
   */
  private def handleGetSR(player: String, jsonNode: JsonNode): Unit = {
    Option(jsonNode.get("id")).filter(!_.isNull) match {
      case Some(idNode) =>
        val id = idNode.asText()
        val outData = JsonToLuaConverter.jsonToLuaTable(getSR(id))
        sendDataToWow(s"SR::$outData")
      case None =>        
        sendDataToWow(CommandResponse("SR", false, player, Some("The pet ate the id")))
        logger.debug("RSR error: missing or null 'id' field")
    }
  }

  /**
   * Handle Delete SR
   */
  private def handleDeleteSR(player: String, jsonNode: JsonNode): Unit = {
    Option(jsonNode.get("id")).filter(!_.isNull) match {
      case Some(idNode) =>
        val id = idNode.asText()
        val outData = deleteSR(id, player)
        sendDataToWow(s"SRDELETE_RESULT::$outData")
      case None =>
        sendDataToWow(CommandResponse("SRDELETE_RESULT", false, player, Some("Oh no, the pet pulled the SR")))
        logger.debug("SRDELETE error: missing or null 'id' field")
    }
  }

  /**
   * Handle Add SR
   */
  private def handleAddSR(player: String, jsonNode: JsonNode): Unit = {
    JsonParsingUtils.parseJsonNode[SRAddData](jsonNode) match {
      case Right(srData) =>
        addSR(srData, player) match {
          case Right(_) =>
            val eventData = getSR(srData.reference)
            val reservationsArray = eventData.get("reservations")

            if (reservationsArray != null && reservationsArray.isArray) {
              val newlyAddedSRs = reservationsArray.asScala.filter { reservation =>
                val characterName = reservation.get("character").get("name").asText()
                val raidItemId = reservation.get("raidItemId").asInt()
              
                characterName == srData.characterName &&
                srData.raidItemIds.contains(raidItemId)
              }.toList
            
              sendDataToWow(CommandResponse("SRADD_RESULT", true, player, None, 
                Map("srId" -> srData.reference, "addedSRs" -> newlyAddedSRs)))
            } else {
              sendDataToWow(CommandResponse("SRADD_RESULT", false, player, Some("ERRCODE 301: Invalid response format")))
            }
          case Left(errorMessage) =>
            sendDataToWow(CommandResponse("SRADD_RESULT", false, player, Some(errorMessage)))
        }
      case Left(errorMessage) =>
        sendDataToWow(CommandResponse("SRADD_RESULT", false, player, Some("ERRCODE 301: SR registration failed")))
        logger.debug(errorMessage)
    }
  }

  /**
   * Handle Request Event
   */
  private def handleRequestEvent(player: String, jsonNode: JsonNode): Unit = {
    Option(jsonNode.get("id")).filter(!_.isNull) match {
      case Some(idNode) =>
        val id = idNode.asText()
        val outData = getEvent(id)
        sendDataToWow(s"EVENT::$outData")
      case None =>
        sendDataToWow(CommandResponse("EVENT", false, player, Some("Where was the raid again?")))
        logger.debug("REVENT error: missing or null 'id' field")
    }
  }

  /**
   * Handle Request Events
   */
  private def handleRequestEvents(player: String): Unit = {
    val outData = getEvents( player )
    sendDataToWow(s"EVENTS::$outData")
  }

  /**
   * Handle Signup
   */
  private def handleSignup(player: String, jsonNode: JsonNode): Unit = {
    JsonParsingUtils.parseJsonNode[SignupData](jsonNode) match {
      case Right(signupData) =>
        val result: SignupResult = checkAccess(player, signupData.channelId)
          .fold(identity, _ => signUp(signupData, player))
        
        sendDataToWow(result.toString)

      case Left(errorMessage) =>
        sendDataToWow(CommandResponse("SIGNUP_RESULT", false, player, Some("ERRCODE 101: Signup failed")))
        logger.debug("ERR 101: ", errorMessage)
    }
  }

  /**
   * Handle Signup Edit
   */
  private def handleSignupEdit(player: String, jsonNode: JsonNode): Unit = {
    JsonParsingUtils.parseJsonNode[EditSignupData](jsonNode) match {                
      case Right(signupData) =>
        val result: SignupResult = checkAccess(player, signupData.channelId)
          .fold(identity, _ => editSignUp(signupData, player))

        sendDataToWow(result.toString)

      case Left(errorMessage) =>
        sendDataToWow(CommandResponse("SIGNUP_RESULT", false, player, Some("ERRCODE 201: Signup edit failed")))
        logger.debug("ERR 202: ", errorMessage)
    }
  }

  /**
   * New Signup
   */
  protected def signUp(data: SignupData, player: String): SignupResult = {
    try {
      val apiKey = Global.config.raid.api_key
      val jsonData = mapper.valueToTree[ObjectNode](data)
      jsonData.put("notify", false)
      val jsonString = mapper.writeValueAsString(jsonData)

      val request = basicRequest
        .post(uri"https://raid-helper.dev/api/v2/events/${data.eventId}/signups")
        .header("Authorization", apiKey)
        .contentType("application/json")
        .body(jsonString)
      val response = request.send(backend)

      response.body.fold(
        error => {
          val jsonNode = mapper.readTree(error)
          SignupResult(
            success = false,
            status = jsonNode.get("error").asText(""),
            player = player
          )
        },
        json => {
          val jsonNode = mapper.readTree(json)
          val eventNode = jsonNode.path("event")
          val signupNode = findSignUp(eventNode, "userId", data.userId)

          signupNode.foreach { node =>
            removeKeys(node, Set("classEmoteId", "specEmoteId"))
          }

          SignupResult(
            success = true,
            status = jsonNode.path("status").asText(),
            player = player,
            eventId = Some(eventNode.path("id").asText()),
            lastUpdated = Some(eventNode.path("lastUpdated").asText()),
            signUp = signupNode
          )
        }
      )
    } catch {
      case e: Exception =>
        logger.error("Error signing up", e)
        SignupResult(
          success = false,
          status = "ERRCODE 102: Signup failed",
          player = player
        )
    }
  }

  /**
   * Edit Signup
   */
  protected def editSignUp(data: EditSignupData, player: String): SignupResult = {
    try {
      val apiKey = Global.config.raid.api_key
      val localMapper = mapper.copy()
      localMapper.setSerializationInclusion(JsonInclude.Include.ALWAYS)
      
      val jsonData = localMapper.valueToTree[ObjectNode](data)
      jsonData.put("notify", false)
      val jsonString = localMapper.writeValueAsString(jsonData)
      
      val request = basicRequest
        .patch(uri"https://raid-helper.dev/api/v2/events/${data.eventId}/signups/${data.signupId}")
        .header("Authorization", apiKey)
        .contentType("application/json")
        .body(jsonString)

      val response = request.send(backend)
      response.body.fold(
        error => {
          val jsonNode = mapper.readTree(error)
          logger.debug("signup edit json error")
          SignupResult(
            success = false,
            status = jsonNode.get("error").asText(""),
            player = player
          )
        },
        json => {
          val jsonNode = mapper.readTree(json)
          val eventNode = jsonNode.path("event")
          val signupNode = findSignUp(eventNode, "id", data.signupId)

          signupNode.foreach { node =>
            removeKeys(node, Set("classEmoteId", "specEmoteId"))
          }

          SignupResult(
            success = true,
            status = jsonNode.path("status").asText(),
            player = player,
            eventId = Some(eventNode.path("id").asText()),
            lastUpdated =  Some(eventNode.path("lastUpdated").asText()),
            signUp = signupNode
          )
        }
      )     
    } catch  {
       case e: Exception =>
        logger.error("Error editing signup", e)
        SignupResult(
          success = false,
          status = "ERRCODE 202: Editing signup failed",
          player = player
        )
    }
  }

  protected def getEvents(player: String): String = {
    try {   
      val api_key = Global.config.raid.api_key
      val server_id = Global.config.raid.server_id
      val timestampSeconds: Long = (System.currentTimeMillis() / 1000) - 43200

      val request = basicRequest
        .get(uri"https://raid-helper.dev/api/v3/servers/$server_id/events")
        .headers(Map(
          "Authorization" -> api_key,
          "StartTimeFilter" -> timestampSeconds.toString,
        ))
        .contentType("application/json")
      val response = request.send(backend)

      response.body match {
        case Right(data) =>
          val jsonNode = mapper.readTree(data)
          val postedEvents = jsonNode.get("postedEvents")

          removeKeys(postedEvents, Set("imageUrl", "description", "channelId" ))

          val root = mapper.createObjectNode()
          root.put("player", player)
          root.put("success", true)
          root.set("events", postedEvents)

          (JsonToLuaConverter.jsonToLuaTable(root))
        case Left(error) =>
          logger.debug(error)
          s"""{success=false, player="$player", status="Invalid response from server"}"""
      }
    } catch  {
       case e: Exception =>
        logger.debug(s"get events error: ${e.getMessage}")
        s"""{success=false, player="$player", status="Error: Unable to get events""""
    }
  }

  protected def getEvent(id: String): String = {
    val request = basicRequest
      .get(uri"https://raid-helper.dev/api/v2/events/$id")
    val response = request.send(backend)

    response.body.fold(
      error_json => {
        val jsonNode = mapper.readTree(error_json)
        val error = jsonNode.get("error").asText("")

        s"""{success=false, status="$error"}"""          
      },
      json => {
        val jsonNode = mapper.readTree(json)
        removeKeys(jsonNode, Set("emoteId", "roleEmoteId", "classEmoteId", "specEmoteId", "advancedSettings" ))
        try {
          val raidId = jsonNode.get("id").asText()
          val title = jsonNode.get("title").asText()
          val start = jsonNode.get("startTime").asLong()
          
          if (Global.data.raidData.addRaid(raidId, title, start)) {            
            Global.game.foreach(_.sendNotification(s"New raid $title on ${Instant.ofEpochSecond(start)}. Check |cffffffff|Hraidcal:event:$raidId|h[RaidCalendar]|h|r or Discord for details and sign up!"))
          }
        } catch {
          case e: Exception =>
            logger.error(s"Error parsing event: ${e.getMessage}")
        }
        
        jsonToLuaTable(jsonNode)
      }
    )
  }

  protected def getSR(id: String): JsonNode = {
    val request = basicRequest
      .get(uri"https://raidres.fly.dev/api/events/$id")
      .contentType("application/json")
      .headers(Map(
        "User-Agent" -> "Bot/RaidCalendar"
      ))
    val response = request.send(backend)
    response.body.fold(
      error_json => {
        val jsonNode = mapper.readTree(error_json)
        val errorNode = mapper.createObjectNode()
        errorNode.put("success", false)
        errorNode.put("status", jsonNode.get("error").asText(""))

        errorNode
      },
      json => {
        val jsonNode = mapper.readTree(json)

        removeKeys(jsonNode, Set("authorId", "eventHubToken", "stale", "lockAtStartTime", "startTime", "minimumAccountAge", 
          "forbiddenUsernames", "allowSrPlus", "previousRaidEventReference", "defaultSrPlusIncrease", "allowSrPlusInput", "extraAdministrators", "hasAuditData",
          "updatedAt", "nameStyle", "isTemporary", "user"
        ))

        // Change srPlus data to only the value
        val reservations = jsonNode.path("reservations").asInstanceOf[ArrayNode]
        for (i <- 0 until reservations.size()) {
          val reservation = reservations.get(i).asInstanceOf[ObjectNode]

          val srPlusNode = reservation.path("srPlus")
          if (!srPlusNode.isMissingNode && srPlusNode.has("value")) {
            val value = srPlusNode.path("value").asInt()
            reservation.put("srPlus", value)
          }
        }

        addCustomSrPlus(jsonNode)
        jsonNode
      }
    )
  }

  protected def addSR(data: SRAddData, player: String): Either[String, Unit] = {
    val cookies = CookieManager.getCookies
    val jsonData = mapper.valueToTree[ObjectNode](data) 
    jsonData.remove("raidId")
    val jsonString = mapper.writeValueAsString(jsonData)

    val request = basicRequest
      .post(uri"https://raidres.fly.dev/api/raid-event-reservations")
      .contentType("application/json")
      .headers(Map(
          "User-Agent" -> "Bot/RaidCalendar",
          "Origin" -> "https://raidres.fly.dev",
          "Cookie" -> cookies,
        ))
      .body(jsonString)

    val response = request.send(backend)
    response.body.fold(
      error_json => {
        val jsonNode = mapper.readTree(error_json)
        val error = jsonNode.get("error").asText("")
        Left(s"ERRCODE 301: SR registration failed - $error")
        
      },
      json => {
        val jsonNode = mapper.readTree(json)
        val message = jsonNode.get("message").asText("")
        if (message == "success") {
          Right(())
        } else {
          Left(s"ERRCODE 301: SR registration failed - $message")
        }
      }
    )
  }

  protected def deleteSR(id: String, player: String): String = {
    val cookies = CookieManager.getCookies
    val request = basicRequest
      .delete(uri"https://raidres.fly.dev/api/raid-event-reservations/$id")
      .contentType("application/json")
      .headers(Map(
          "User-Agent" -> "Bot/RaidCalendar",
          "Origin" -> "https://raidres.fly.dev",
          "Cookie" -> cookies,
        ))
      .body(id)

    val response = request.send(backend)
    response.body.fold(
      error_json => {
        logger.debug(s"failed: $error_json")
        val jsonNode = mapper.readTree(error_json)
        val error = jsonNode.get("error").asText("")
        logger.debug("delete sr error")
        s"""{success=false, id=$id, player="$player", status="$error"}"""
      },
      json => {
        val jsonNode = mapper.readTree(json)
        val success = (jsonNode.get("message").asText("") == "success")

        s"""{success=$success, id=$id, player="$player"}"""
      }
    )
  }

  protected def checkAccess(player: String, channelId: String): Either[SignupResult, Unit] = {
    val denied = SignupResult(
      success = false,
      status = "Access denied",
      player = player
    )

    Global.data.userData.getChannelValue(player, channelId) match {
      case Some(true)  => Right(())
      case Some(false) => Left(denied)
      case None =>
        if (Global.discord.hasChannelAccess(player, channelId, false)) {
          Global.data.userData.addChannelToPlayer(player, channelId)
          Right(())
        } else Left(denied)
    }
  }

  protected def getJoke(name: Option[String]): Unit = {
    val request = basicRequest
      .get(uri"https://official-joke-api.appspot.com/random_joke")
      .contentType("application/json")
    val response = request.send(backend)

    response.body match {
      case Right(data) =>
        val jsonNode = mapper.readTree(data)

        logger.info(s"Telling a joke to $name")
        if (name.contains("GUILD")) {
          Global.game.foreach(_.sendNotification(jsonNode.get("setup").asText()))
        } else {
          Global.game.foreach(_.sendMessageToWow(ChatEvents.CHAT_MSG_WHISPER, jsonNode.get("setup").asText(), name))
        }

        val punchline = jsonNode.get("punchline").asText()

        reconnectExecutor.schedule(new Runnable {
          override def run(): Unit = {
            if (name.contains("GUILD")) {
              Global.game.foreach(_.sendNotification(punchline))
            } else {
              Global.game.foreach(_.sendMessageToWow(ChatEvents.CHAT_MSG_WHISPER, punchline, name))
            }
          }
        }, 3, TimeUnit.SECONDS)
      case Left(error) =>
        logger.debug(error)
        Global.game.foreach(_.sendMessageToWow(ChatEvents.CHAT_MSG_WHISPER, "Oh no, something is not right", name))
    }
  }

  protected def findSignUp(jsonNode: JsonNode, nodeName: String, idToFind: String): Option[JsonNode] = {
    val signUpsNode = jsonNode.get("signUps")
    if (signUpsNode != null && signUpsNode.isArray) {
      signUpsNode.iterator().asScala.find { node =>
        val idNode = node.get(nodeName)
        idNode != null && idNode.asText() == idToFind
      }
    } else {
      None
    }
  }

  protected def sendDataToWow(data: String) = {
    val pattern = "(.*?)::".r
    val maybeName = pattern.findFirstMatchIn(data).map(_.group(1))
    logger.info(s"Sending ${maybeName.getOrElse("Not found")}")

    if (data.length > 240) {
      val chunks = data.grouped(240).toList

      Global.game.foreach(_.sendAddonMessageToWow(ChatEvents.CHAT_MSG_GUILD, s"CT::${chunks.length}", "RaidCal"))

      chunks.zipWithIndex.foreach { case (chunk, index) =>
        Thread.sleep(50)
        val chunkId = index + 1
        Global.game.foreach(_.sendAddonMessageToWow(ChatEvents.CHAT_MSG_GUILD, s"C$chunkId::$chunk", "RaidCal"))
      }
    } else {
      Global.game.foreach(_.sendAddonMessageToWow(ChatEvents.CHAT_MSG_GUILD, data, "RaidCal"))
    }
  }

  protected def convertToCurrency(copperAmount: Int): Currency = {
    val gold = copperAmount / 10000
    val remaining = copperAmount % 10000
    val silver = remaining / 100
    val copper = remaining % 100
    Currency(gold, silver, copper)
  }

  protected def parseJsonAndCalculateAverage(jsonString: String): Option[Currency] = {
    val rootNode: JsonNode = mapper.readTree(jsonString)
    
    val avgPrices = collection.mutable.ListBuffer[Long]()
    val iterator = rootNode.fields()
    while (iterator.hasNext) {
      val entry = iterator.next()
      val priceData = entry.getValue
      val avgPriceNode = priceData.get("avg_price")

      if (avgPriceNode != null && !avgPriceNode.isNull) {
        avgPrices += priceData.get("avg_price").asLong()
      }
    }
    
    if (avgPrices.nonEmpty) {
      val averageCopper = (avgPrices.sum.toDouble / avgPrices.length).round.toInt
      val currency = convertToCurrency(averageCopper)
      if (currency == Currency(0, 0, 0)) None else Some(currency)
    } else {
      None
    }
  }

  protected def priceCheck(input: String): String = {
    val itemPattern = "\\|Hitem:(\\d+):".r

    itemPattern.findFirstMatchIn(input).map(_.group(1).toInt) match {
      case Some(itemId) =>
        var source: Option[Source] = None
        try {
          source = Some(Source.fromURL(s"https://api.wowauctions.net/items/stats/7d/nordanaar/mergedAh/$itemId"))
          val jsonString = source.get.mkString

          parseJsonAndCalculateAverage(jsonString) match {
            case Some(averagePrice) => s"Average price for $input last week was $averagePrice."
            case None => s"No price information found for $input"
          }
        } catch {
          case e: Exception =>
            logger.debug(e.getMessage)
            ( "Something went wrong, I blame the pet" )
        } finally {
          source.foreach(_.close())
        }
      case None => "No valid item link found"
    }
  }

  private object CommandResponse {
    def apply(command: String, success: Boolean, player: String, status: Option[String] = None, extra: Map[String, Any] = Map.empty): String = {
      val statusField = status.map(s => s"""status="$s"""").toSeq
      val extraFields = extra.map { 
        case (k, v: List[_]) if v.forall(_.isInstanceOf[JsonNode]) =>
          val nodes = v.asInstanceOf[List[JsonNode]]
          s"""$k={${nodes.map(JsonToLuaConverter.jsonToLuaTable).mkString(",")}}"""
        case (k, v: JsonNode) => s"""$k=${JsonToLuaConverter.jsonToLuaTable(v)}"""
        case (k, v) => s"""$k="$v""""
      }.toSeq
      val allFields = Seq(
        s"success=$success",
        s"""player="$player""""
      ) ++ statusField ++ extraFields
      
      s"""${command}::{${allFields.mkString(",")}}"""
    }
  }

  object JsonParsingUtils {
    def parseJsonNode[T](jsonNode: JsonNode)(implicit ct: ClassTag[T]): Either[String, T] = {
      Try(mapper.treeToValue(jsonNode, ct.runtimeClass.asInstanceOf[Class[T]])) match {
        case Success(data) => Right(data)
        case Failure(e)    => Left(s"Failed to parse ${ct.runtimeClass.getSimpleName}: ${e.getMessage}")
      }
    }
  }

  def addCustomSrPlus(node: JsonNode ): Unit = {
    if (node.isObject) {
      val objectNode = node.asInstanceOf[ObjectNode]      
      val raidId = node.get("raidId").asText("")

      val srSource = Source.fromURL(s"https://wow.cook.as/fox/data.php?action=items&id=$raidId")
      val srJson = try srSource.mkString finally srSource.close()
      val srPlussNode = mapper.readTree(srJson)
      
      if ( srPlussNode.path("id").asText("") == objectNode.path("reference").asText("")) {
        enrichReservationsWithSrPlus(objectNode, srPlussNode)
      }
    }
  }


  def enrichReservationsWithSrPlus(originalNode: ObjectNode, srPlussNode: JsonNode): Unit = {
    val reservations = originalNode.path("reservations")
    if (!reservations.isArray) return

    val srArray = srPlussNode.path("sr")
    if (!srArray.isArray) return

    for (reservation <- reservations.elements().asScala) {
      val raidItemId = reservation.path("raidItemId").asInt()
      val characterName = reservation.path("character").path("name").asText()

      // Find matching sr entry
      val matchingSr = srArray.elements().asScala.find { srEntry =>
        val srItemId = srEntry.path("raidItemId").asInt()
        val characters = srEntry.path("characters")
        srItemId == raidItemId && characters.has(characterName)
      }

      // If found, extract the SR+ value and insert
      matchingSr.foreach { srEntry =>
        val newSrPlusValue = srEntry.path("characters").path(characterName).asInt() * 10
        val reservationObj = reservation.asInstanceOf[ObjectNode]
        val existingSrPlus = reservationObj.path("srPlus")

        val combinedValue = if (existingSrPlus.isInt) {
          existingSrPlus.asInt() + newSrPlusValue
        } else {
          newSrPlusValue
        }

        reservationObj.put("srPlus", combinedValue)
      }
    }
  }

  object ReminderScheduler {
    private val scheduler: ScheduledExecutorService = Executors.newScheduledThreadPool(1)

    def scheduleReminder(reminder: ReminderCheck): Unit = {
      if (!reminder.enabled) return

      scheduler.scheduleAtFixedRate(
        new Runnable {
          def run(): Unit = {
            if (reminder.messages.nonEmpty) {
              val msg = reminder.messages(Random.nextInt(reminder.messages.size))
              Global.game.foreach(_.sendNotification(msg))
            }
          }
        },
        reminder.offsetMinutes.toLong,
        reminder.intervalMinutes.toLong,
        TimeUnit.MINUTES
      )
    }
  }

  object CookieManager extends StrictLogging {
    private val cookieFile = "cookies.txt"
    private val expiryThresholdSeconds = 60 * 60 * 24 * 6 // 6 days

    def getCookies(): String = {
      val savedHeaders = loadSetCookieHeaders()

      if (savedHeaders.nonEmpty) {
        logger.debug("We have cookies")
        if (needsRefresh(savedHeaders)) {
          logger.debug("refresh cookies")
          refreshAndSaveCookies(savedHeaders)
        } else {
          logger.debug("return cookies")
          cookieHeaderValue(savedHeaders)
        }
      } else {
        logger.debug("No cookies")
        refreshAndSaveCookies(Seq.empty)
      }
    }

    private def saveSetCookieHeaders(cookies: Seq[String]): Unit = {
      val content = cookies.mkString("\n")
      Files.write(Paths.get(cookieFile), content.getBytes(StandardCharsets.UTF_8))
    }

    private def loadSetCookieHeaders(): Seq[String] = {
      val p = Paths.get(cookieFile)
      if (Files.exists(p)) Source.fromFile(p.toFile).getLines().toSeq else Seq.empty
    }

    private def parseSetCookieHeaders(headers: Seq[String]): Seq[CookieWithMeta] =
      headers.flatMap(h => CookieWithMeta.parse(h).toOption)  

    private def cookieHeaderValue(headers: Seq[String]): String =
      headers.map(_.split(";").head.trim).mkString("; ")

    private def needsRefresh(headers: Seq[String]): Boolean = {
      val now = Instant.now()

      // Parse all cookies that can be parsed
      val cookies = headers.flatMap(h => CookieWithMeta.parse(h).toOption)

      // If any cookie expires within threshold, refresh needed
      cookies.exists { c =>
        c.expires match {
          case Some(expiry) => expiry.isBefore(now.plusSeconds(expiryThresholdSeconds))
          case None => true // If expiry missing, better to refresh to be safe
        }
      }
    }

    private def refreshAndSaveCookies(currentHeaders: Seq[String]): String = {
      val fallbackCookies = if (currentHeaders.nonEmpty) cookieHeaderValue(currentHeaders) else Global.config.raid.cookies

      logger.debug("Sending auth request")
      val authRequest = basicRequest
        .get(uri"https://raidres.fly.dev/api/auth/me")
        .headers(Map(
          "User-Agent" -> "Bot/RaidCalendar",
          "Origin" -> "https://raidres.fly.dev",
          "Cookie" -> fallbackCookies
        ))

      val authResponse = authRequest.send(backend)
      val newSetCookieHeaders = authResponse.headers("Set-Cookie").toList

      if (newSetCookieHeaders.nonEmpty) {
        val cookieNames: Set[String] = newSetCookieHeaders.map { cookie => cookie.takeWhile(_ != '=') }.toSet
        val requiredCookies = Set("d.refresh_token", "d.expiry", "jwt", "d.access_token")
        logger.debug( cookieNames.mkString(", ") )
        logger.debug( newSetCookieHeaders.mkString(", ") )

        if (requiredCookies.diff(cookieNames).isEmpty) {
          saveSetCookieHeaders(newSetCookieHeaders)
        }
        cookieHeaderValue(newSetCookieHeaders)
      } else {      
        if (currentHeaders.nonEmpty) {
          cookieHeaderValue(currentHeaders)
        } else {
          Global.config.raid.cookies
        }
      }
    }
  }
}
