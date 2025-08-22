package raidcalbot.common

import java.io.File
import java.util

import raidcalbot.common.ChatDirection.ChatDirection
import raidcalbot.common.WowExpansion.WowExpansion
import com.typesafe.config.{Config, ConfigFactory}
import raidcalbot.game.GamePackets

import scala.collection.JavaConverters._
import scala.reflect.runtime.universe.{TypeTag, typeOf}

case class RaidCalBotConfig(discord: DiscordConfig, raid: Raid, wow: Wow)
case class DiscordConfig(token: String)
case class Raid(api_key: String, server_id: String, cookies: String, announceNewRaids: Boolean, cookSrPlus: Boolean)
case class Wow(locale: String, platform: Platform.Value, realmBuild: Option[Int], gameBuild: Option[Int], realmlist: RealmListConfig, account: Array[Byte], password: String, character: String, enableServerMotd: Boolean, hydrationCheck: ReminderCheck, postureCheck: ReminderCheck)
case class RealmListConfig(name: String, host: String, port: Int)
case class ReminderCheck(enabled: Boolean, intervalMinutes: Int, offsetMinutes: Int, messages: Seq[String])

object RaidCalBotConfig extends GamePackets {

  private var version: String = _
  private var expansion: WowExpansion = _

  def apply(confFile: String): RaidCalBotConfig = {
    val file = new File(confFile)
    val config = (if (file.exists) {
      ConfigFactory.parseFile(file)
    } else {
      ConfigFactory.load(confFile)
    }).resolve

    val discordConf = config.getConfig("discord")
    val raidHelperConf = config.getConfig("raidhelper")
    val raidResConf = config.getConfig("raidres")
    val wowConf = config.getConfig("wow")

    val hydrationConf = wowConf.getConfig("hydration_check")
    val postureConf = wowConf.getConfig("posture_check")

    // we gotta load this first to initialize constants that change between versions :OMEGALUL:
    version = getOpt(wowConf, "version").getOrElse("1.12.1")
    expansion = WowExpansion.valueOf(version)

    RaidCalBotConfig(
      DiscordConfig(
        discordConf.getString("token")
      ),
      Raid(
        raidHelperConf.getString("api_key"),
        raidHelperConf.getString("server_id"),        
        raidResConf.getString("cookies"),
        getOpt[Boolean](raidHelperConf, "announce_new_raids").getOrElse(true),
        getOpt[Boolean](raidResConf, "cook_sr_plus").getOrElse(false)
      ),
      Wow(
        getOpt[String](wowConf, "locale").getOrElse("enUS"),
        Platform.valueOf(getOpt[String](wowConf, "platform").getOrElse("Mac")),
        getOpt[Int](wowConf, "realm_build").orElse(getOpt[Int](wowConf, "build")),
        getOpt[Int](wowConf, "game_build").orElse(getOpt[Int](wowConf, "build")),
        parseRealmlist(wowConf),
        convertToUpper(wowConf.getString("account")),
        wowConf.getString("password"),
        wowConf.getString("character"),
        getOpt[Boolean](wowConf, "enable_server_motd").getOrElse(true),
        ReminderCheck(
          getOpt[Boolean](hydrationConf, "enabled").getOrElse(false),
          hydrationConf.getInt("interval_minutes"),
          hydrationConf.getInt("offset_minutes"),
          hydrationConf.getStringList("messages").asScala.toSeq
        ),
        ReminderCheck(
          getOpt[Boolean](postureConf, "enabled").getOrElse(false),
          postureConf.getInt("interval_minutes"),
          postureConf.getInt("offset_minutes"),
          postureConf.getStringList("messages").asScala.toSeq
        )
      )
    )
  }

  lazy val getVersion = version
  lazy val getExpansion = expansion

  private lazy val buildFromVersion: Int =
    version match {
      case "1.6.1" => 4544
      case "1.6.2" => 4565
      case "1.6.3" => 4620
      case "1.7.1" => 4695
      case "1.8.4" => 4878
      case "1.9.4" => 5086
      case "1.10.2" => 5302
      case "1.11.2" => 5464
      case "1.12.1" => 5875
      case "1.12.2" => 6005
      case "1.12.3" => 6141
      case "2.4.3" => 8606
      case "3.2.2" => 10505
      case "3.3.0" => 11159
      case "3.3.2" => 11403
      case "3.3.3" => 11723
      case "3.3.5" => 12340
      case "4.3.4" => 15595
      case "5.4.8" => 18414
      case _ => throw new IllegalArgumentException(s"Build $version not supported!")
    }

  lazy val getRealmBuild: Int = Global.config.wow.realmBuild.getOrElse(buildFromVersion)
  lazy val getGameBuild: Int = Global.config.wow.gameBuild.getOrElse(buildFromVersion)

  private def convertToUpper(account: String): Array[Byte] = {
    account.map(c => {
      if (c >= 'a' && c <= 'z') {
        c.toUpper
      } else {
        c
      }
    }).getBytes("UTF-8")
  }

  private def parseRealmlist(wowConf: Config): RealmListConfig = {
    val realmlist = wowConf.getString("realmlist")
    val splt = realmlist.split(":", 2)
    val (host, port) =
      if (splt.length == 1) {
        (splt.head, 3724)
      } else {
        (splt.head, splt(1).toInt)
      }

    RealmListConfig(wowConf.getString("realm"), host, port)
  }

  private def getConfigOpt(cfg: Config, path: String): Option[Config] = {
    if (cfg.hasPath(path)) {
      Some(cfg.getConfig(path))
    } else {
      None
    }
  }

  private def getOpt[T : TypeTag](cfg: Config, path: String): Option[T] = {
    if (cfg.hasPath(path)) {
      // evil smiley face :)
      Some(
        (if (typeOf[T] =:= typeOf[Boolean]) {
          cfg.getString(path).toLowerCase match {
            case "true" | "1" | "y" | "yes" => true
            case _ => false
          }
        } else if (typeOf[T] =:= typeOf[String]) {
          cfg.getString(path)
        } else {
          cfg.getAnyRef(path)
        }).asInstanceOf[T]
      )
    } else {
      None
    }
  }
}

object Platform extends Enumeration {
  type Platform = Value
  val Windows, Mac = Value

  def valueOf(platform: String): Platform = {
    platform.toLowerCase match {
      case "win" | "windows" => Windows
      case _ => Mac
    }
  }
}

object WowExpansion extends Enumeration {
  type WowExpansion = Value
  val Vanilla, TBC, WotLK, Cataclysm, MoP = Value

  def valueOf(version: String): WowExpansion = {
    if (version.startsWith("1.")) {
      WowExpansion.Vanilla
    } else if (version.startsWith("2.")) {
      WowExpansion.TBC
    } else if (version.startsWith("3.")) {
      WowExpansion.WotLK
    } else if (version == "4.3.4") {
      WowExpansion.Cataclysm
    } else if (version == "5.4.8") {
      WowExpansion.MoP
    } else {
      throw new IllegalArgumentException(s"Version $version not supported!")
    }
  }
}

object ChatDirection extends Enumeration {
  type ChatDirection = Value
  val both, wow_to_discord, discord_to_wow = Value
}
