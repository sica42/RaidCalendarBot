package raidcalbot.common

import com.typesafe.scalalogging.StrictLogging
import com.fasterxml.jackson.databind.{JsonNode, ObjectMapper}
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.fasterxml.jackson.core.`type`.TypeReference
import scala.collection.mutable
import scala.util.{Try, Success, Failure}
import java.io.{File, FileWriter}
import java.time.Instant
import scala.collection.JavaConverters._

case class UserInfo(discordUserId: String, channelIds: mutable.Map[String, Boolean] = mutable.Map.empty)
case class RaidInfo(title: String, startTime: Instant)

object RaidCalBotDataManager {
  private val mapper = new ObjectMapper()
  mapper.registerModule(DefaultScalaModule)

  def apply(dataFile: String): RaidCalBotDataManager = {
    new RaidCalBotDataManager(dataFile)
  }
}

class RaidCalBotDataManager(dataFile: String) extends StrictLogging {
  private val mapper = RaidCalBotDataManager.mapper
  
  // Initialize sub-managers
  val userData = new UserDataManager(this)
  val raidData = new RaidDataManager(this)
  
  // Load all data on initialization
  loadData()
  
  private def loadData(): Unit = {
    val file = new File(dataFile)
    if (file.exists()) {
      Try {
        val rootNode = mapper.readTree(file)
        
        // Load user data
        if (rootNode.has("users") && rootNode.get("users").isArray) {
          val userMap = mutable.Map.empty[String, UserInfo]
          rootNode.get("users").elements().asScala.foreach { userNode =>
            val playerName = userNode.get("playerName").asText()
            val discordUserId = userNode.get("discordUserId").asText()
            val channelIds = mutable.Map.empty[String, Boolean]
            
            if (userNode.has("channelIds") && userNode.get("channelIds").isObject) {
              userNode.get("channelIds").fields().asScala.foreach { entry =>
                channelIds += entry.getKey -> entry.getValue.asBoolean()
              }
            }
            
            userMap(playerName) = UserInfo(discordUserId, channelIds)
          }
          userData.loadUsers(userMap)
        }
        
        // Load raid data
        if (rootNode.has("raids") && rootNode.get("raids").isArray) {
          val raidMap = mutable.Map.empty[String, RaidInfo]
          rootNode.get("raids").elements().asScala.foreach { raidNode =>
            val raidId = raidNode.get("raidId").asText()
            val title = raidNode.get("title").asText()
            val startTime = Instant.ofEpochSecond(raidNode.get("startTime").asLong())

            raidMap(raidId) = RaidInfo(title, startTime)
          }
          raidData.loadRaids(raidMap)
        }
        
      } match {
        case Success(_) => logger.info(s"Successfully loaded data from $dataFile")
        case Failure(e) => logger.error(s"Failed to load data: ${e.getMessage}")
      }
    } else {
      logger.info(s"File $dataFile not found, starting with empty data")
    }
  }
  
  def saveData(): Unit = {
    Try {
      raidData.cleanupOldRaids()

      val rootNode = mapper.createObjectNode()
      
      // Save user data
      val usersArray = mapper.createArrayNode()
      userData.getAllUsers.foreach { case (playerName, userInfo) =>
        val userNode = mapper.createObjectNode()
        userNode.put("playerName", playerName)
        userNode.put("discordUserId", userInfo.discordUserId)
        
        val channelIdsObject = mapper.createObjectNode()
        userInfo.channelIds.foreach { case (channelId, value) =>
          channelIdsObject.put(channelId, value)
        }
        userNode.set("channelIds", channelIdsObject)
        usersArray.add(userNode)
      }
      rootNode.set("users", usersArray)
      
      // Save raid data
      val raidsArray = mapper.createArrayNode()
      raidData.getAllRaids.foreach { case (raidId, raidInfo) =>
        val raidNode = mapper.createObjectNode()
        raidNode.put("raidId", raidId)
        raidNode.put("title", raidInfo.title)
        raidNode.put("startTime", raidInfo.startTime.getEpochSecond)

        raidsArray.add(raidNode)
      }
      rootNode.set("raids", raidsArray)
      
      val writer = new FileWriter(dataFile)
      try {
        mapper.writerWithDefaultPrettyPrinter().writeValue(writer, rootNode)
      } finally {
        writer.close()
      }
      
    } match {
      case Success(_) => logger.info(s"Successfully saved data to $dataFile")
      case Failure(e) => logger.error(s"Failed to save data: ${e.getMessage}")
    }
  }
}

class UserDataManager(dataManager: RaidCalBotDataManager) {
  private val userData: mutable.Map[String, UserInfo] = mutable.Map.empty
  
  private[common] def loadUsers(users: mutable.Map[String, UserInfo]): Unit = {
    userData.clear()
    userData ++= users
  }
  
  def getUser(playerName: String): Option[UserInfo] = {
    userData.get(playerName)
  }
  
  def addUser(playerName: String, discordUserId: String): Boolean = {
    if (userData.contains(playerName)) {
      false
    } else {
      val channelIds = mutable.Map.empty[String, Boolean]
      userData(playerName) = UserInfo(discordUserId, channelIds)
      dataManager.saveData()
      true
    }
  }
  
  def addChannelToPlayer(playerName: String, channelId: String, value: Boolean = true): Boolean = {
    userData.get(playerName) match {
      case Some(userInfo) =>
        if (userInfo.channelIds.contains(channelId)) {
          false
        } else {
          userInfo.channelIds(channelId) = value
          dataManager.saveData()
          true
        }
      case None => false
    }
  }
  
  def getChannelValue(playerName: String, channelId: String): Boolean = {
    userData.get(playerName).flatMap(_.channelIds.get(channelId)).getOrElse(false)
  }
  
  def updateChannelValue(playerName: String, channelId: String, value: Boolean): Boolean = {
    userData.get(playerName) match {
      case Some(userInfo) =>
        if (userInfo.channelIds.contains(channelId)) {
          userInfo.channelIds(channelId) = value
          dataManager.saveData()
          true
        } else {
          false
        }
      case None => false
    }
  }
  
  def getAllUsers: Map[String, UserInfo] = userData.toMap
}

class RaidDataManager(dataManager: RaidCalBotDataManager) {
  private val raidData: mutable.Map[String, RaidInfo] = mutable.Map.empty
  
  private[common] def loadRaids(raids: mutable.Map[String, RaidInfo]): Unit = {
    raidData.clear()
    raidData ++= raids
  }

   private[common] def cleanupOldRaids(): Unit = {
    val cutoffTime = Instant.now().minusSeconds(24 * 60 * 60) // 24 hours ago
    val oldRaidIds = raidData.filter { case (_, raidInfo) =>
      raidInfo.startTime.isBefore(cutoffTime)
    }.keys.toList
    
    oldRaidIds.foreach(raidData.remove)
    
    if (oldRaidIds.nonEmpty) {
      // Add logging to track cleanup
      println(s"Cleaned up ${oldRaidIds.size} old raids: ${oldRaidIds.mkString(", ")}")
    }
  }
  
  def getRaid(raidId: String): Option[RaidInfo] = {
    raidData.get(raidId)
  }
  
  def addRaid(raidId: String, title:String, startTime: Long): Boolean = {
    if (raidData.contains(raidId)) {
      false
    } else {
      raidData(raidId) = RaidInfo(title, Instant.ofEpochSecond(startTime))
      dataManager.saveData()
      true
    }
  }
  
  def getAllRaids: Map[String, RaidInfo] = raidData.toMap
}