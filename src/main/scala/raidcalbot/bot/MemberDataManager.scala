package raidcalbot.bot

import com.typesafe.scalalogging.StrictLogging
import com.fasterxml.jackson.databind.{JsonNode, ObjectMapper}
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.fasterxml.jackson.core.`type`.TypeReference
import scala.collection.mutable
import scala.util.{Try, Success, Failure}
import java.io.{File, FileWriter}
import scala.collection.JavaConverters._

case class MemberInfo(discordUserId: String, channelIds: mutable.Map[String, Boolean] = mutable.Map.empty)

object MemberDataManager {
  private var _instance: Option[MemberDataManager] = None
  
  def getInstance(filePath: String = "members.json"): MemberDataManager = {
    _instance match {
      case Some(manager) => manager
      case None =>
        val manager = new MemberDataManager(filePath)
        _instance = Some(manager)
        manager
    }
  }
}

class MemberDataManager(filePath: String) extends StrictLogging {
  private val mapper = new ObjectMapper()
  mapper.registerModule(DefaultScalaModule)
  
  // Internal storage: playerName -> MemberInfo
  private val memberData: mutable.Map[String, MemberInfo] = mutable.Map.empty
  
  // Load data on initialization
  loadData()
  
  private def loadData(): Unit = {
    val file = new File(filePath)
    if (file.exists()) {
      Try {
        val jsonNode = mapper.readTree(file)
        if (jsonNode.isArray) {
          jsonNode.elements().asScala.foreach { memberNode =>
            val playerName = memberNode.get("playerName").asText()
            val discordUserId = memberNode.get("discordUserId").asText()
            val channelIds = mutable.Map.empty[String, Boolean]
            
            if (memberNode.has("channelIds") && memberNode.get("channelIds").isObject) {
              memberNode.get("channelIds").fields().asScala.foreach { entry =>
                channelIds += entry.getKey -> entry.getValue.asBoolean()
              }
            }
            
            memberData(playerName) = MemberInfo(discordUserId, channelIds)
          }
        }
      } match {
        case Success(_) => logger.info(s"Successfully loaded member data from $filePath")
        case Failure(e) => logger.info(s"Failed to load member data: ${e.getMessage}")
      }
    }
  }
  
  private def saveData(): Unit = {
    Try {
      val arrayNode = mapper.createArrayNode()
      
      memberData.foreach { case (playerName, memberInfo) =>
        val memberNode = mapper.createObjectNode()
        memberNode.put("playerName", playerName)
        memberNode.put("discordUserId", memberInfo.discordUserId)
        
        val channelIdsObject = mapper.createObjectNode()
        memberInfo.channelIds.foreach { case (channelId, value) =>
          channelIdsObject.put(channelId, value)
        }
        memberNode.set("channelIds", channelIdsObject)
        
        arrayNode.add(memberNode)
      }
      
      val writer = new FileWriter(filePath)
      try {
        mapper.writerWithDefaultPrettyPrinter().writeValue(writer, arrayNode)
      } finally {
        writer.close()
      }
    } match {
      case Success(_) => logger.info(s"Successfully saved member data to $filePath")
      case Failure(e) => logger.info(s"Failed to save member data: ${e.getMessage}")
    }
  }

  /**
   * Gets the boolean value for a specific channel ID for a player
   */
  def getChannelValue(playerName: String, channelId: String): Boolean = {
    memberData.get(playerName).flatMap(_.channelIds.get(channelId)).getOrElse(false)
  }
  
  /**
   * Adds a new user with player name and initial channel ID
   */
  def addUser(playerName: String, discordUserId: String): Boolean = {
    if (memberData.contains(playerName)) {
      logger.debug(s"Player $playerName already exists")
      false
    } else {
      val channelIds = mutable.Map.empty[String, Boolean]
      memberData(playerName) = MemberInfo(discordUserId, channelIds)
      saveData()
      logger.debug(s"Added new user: $playerName")
      true
    }
  }
  
  /**
   * Adds a new channel ID to an existing player's channel list with a boolean value
   */
  def addChannelToPlayer(playerName: String, channelId: String, value: Boolean = true): Boolean = {
    memberData.get(playerName) match {
      case Some(memberInfo) =>
        if (memberInfo.channelIds.contains(channelId)) {
          logger.debug(s"Channel $channelId already exists for player $playerName")
          false
        } else {
          memberInfo.channelIds(channelId) = value
          saveData()
          logger.debug(s"Added channel $channelId with value $value to player $playerName")
          true
        }
      case None =>
        logger.debug(s"Player $playerName not found")
        false
    }
  }

  /**
   * Updates the boolean value for an existing channel ID for a player
   */
  def updateChannelValue(playerName: String, channelId: String, value: Boolean): Boolean = {
    memberData.get(playerName) match {
      case Some(memberInfo) =>
        if (memberInfo.channelIds.contains(channelId)) {
          memberInfo.channelIds(channelId) = value
          saveData()
          logger.debug(s"Updated channel $channelId to value $value for player $playerName")
          true
        } else {
          logger.debug(s"Channel $channelId not found for player $playerName")
          false
        }
      case None =>
        logger.debug(s"Player $playerName not found")
        false
    }
  }
  
  def getAllMembers: Map[String, MemberInfo] = memberData.toMap

  def getMember(playerName: String): Option[MemberInfo] = {
    memberData.get(playerName)
  }
}