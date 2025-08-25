package raidcalbot.discord

import raidcalbot.common._
import com.typesafe.scalalogging.StrictLogging
import com.vdurmont.emoji.EmojiParser
import net.dv8tion.jda.api.JDABuilder
import net.dv8tion.jda.api.JDA.Status
import net.dv8tion.jda.api.Permission
import net.dv8tion.jda.api.entities.{Activity, MessageType}
import net.dv8tion.jda.api.entities.Activity.ActivityType
import net.dv8tion.jda.api.entities.channel.ChannelType
import net.dv8tion.jda.api.events.StatusChangeEvent
import net.dv8tion.jda.api.events.session.ShutdownEvent
import net.dv8tion.jda.api.events.message.MessageReceivedEvent
import net.dv8tion.jda.api.events.interaction.component.ButtonInteractionEvent
import net.dv8tion.jda.api.hooks.ListenerAdapter
import net.dv8tion.jda.api.requests.{CloseCode, GatewayIntent}
import net.dv8tion.jda.api.utils.MemberCachePolicy
import net.dv8tion.jda.api.utils.cache.CacheFlag
import net.dv8tion.jda.api.EmbedBuilder
import net.dv8tion.jda.api.interactions.components.ActionRow;
import net.dv8tion.jda.api.interactions.components.buttons.Button

import java.awt.Color
import raidcalbot.game.GamePackets

import scala.collection.JavaConverters._
import scala.collection.mutable

class Discord(discordConnectionCallback: CommonConnectionCallback) extends ListenerAdapter
  with GamePackets with StrictLogging {

  private val jda = JDABuilder
    .createDefault(Global.config.discord.token, GatewayIntent.GUILD_EXPRESSIONS, GatewayIntent.GUILD_MEMBERS, GatewayIntent.GUILD_MESSAGES, GatewayIntent.GUILD_PRESENCES, GatewayIntent.DIRECT_MESSAGES, GatewayIntent.MESSAGE_CONTENT)
    .setMemberCachePolicy(MemberCachePolicy.ALL)
    .disableCache(CacheFlag.SCHEDULED_EVENTS, CacheFlag.VOICE_STATE)
    .addEventListeners(this)
    .build

  private val messageResolver = MessageResolver(jda)

  private var lastStatus: Option[Activity] = None
  private var firstConnect = true

  @volatile private var isConnected = false
  def isBotConnected: Boolean = isConnected

  def changeStatus(gameType: ActivityType, message: String): Unit = {
    lastStatus = Some(Activity.of(gameType, message))
    jda.getPresence.setActivity(lastStatus.get)
  }

  def changeRealmStatus(message: String): Unit = {
    changeStatus(ActivityType.CUSTOM_STATUS, message)
  }

  def getBotName(): String = {
    jda.getSelfUser.getName
  }

  def getUserId(name: String): Option[String] = {
    val guildId = Global.config.raid.server_id
    for {
      guild  <- Option(jda.getGuildById(guildId))
      member <- guild.getMembers.asScala.find { m =>
        val inputName = name.stripPrefix("@")

        m.getEffectiveName.equalsIgnoreCase(inputName) ||
        Option(m.getNickname).exists(_.equalsIgnoreCase(inputName)) ||
        m.getUser.getName.equalsIgnoreCase(inputName)
      }
    } yield member.getId
  }

  def hasChannelAccess(player: String, channelId: String, renew: Boolean = false): Boolean = {
    Global.data.userData.getUser(player) match {
      case Some(memberInfo) if !renew && memberInfo.channelIds.contains(channelId) =>
        memberInfo.channelIds(channelId)

      case Some(memberInfo) =>
        val channelOpt = Option(jda.getGuildChannelById(channelId))
        val success = channelOpt.flatMap { channel =>
          Option(channel.getGuild.getMemberById(memberInfo.discordUserId)).map { member =>
            member.hasPermission(channel, Permission.VIEW_CHANNEL)
          }
        }.getOrElse(false)

        if (memberInfo.channelIds.contains(channelId)) {
          Global.data.userData.updateChannelValue(player, channelId, success)
        } else {
          Global.data.userData.addChannelToPlayer(player, channelId, success)
        }
        success
        
      case None =>
        false
    }
  }

  def authorizeUser(userId: String, player: String): Unit = {
    jda.retrieveUserById(userId).queue { user =>
      user.openPrivateChannel().queue { privateChannel =>
        val embed = new EmbedBuilder()
          .setTitle(s"""Authorize character "`$player`" for use with RaidCalendar?""")
          .setDescription(s"This will allow **$player** to register for events using your account in **RaidCalendar**.")
          .setColor(Color.CYAN)

        val yesButton = Button.success(s"allow:$player", "Allow")
        val noButton = Button.danger(s"deny:$player", "Deny")

        privateChannel.sendMessageEmbeds(embed.build())
          .setActionRow(yesButton, noButton)
          .queue()
      }
    }
  }

  override def onButtonInteraction(event: ButtonInteractionEvent): Unit = {
    val userId: String = event.getUser.getId
    val Array(action, player) = event.getComponentId.split(":", 2)

    val message = event.getMessage
    val yesBtn = Button.success(s"allow:$player", "Allow").asDisabled()
    val noBtn = Button.danger(s"deny:$player", "Deny").asDisabled()

    message.editMessageComponents(ActionRow.of(yesBtn, noBtn)).queue()

    action match {
      case "allow" =>
        event.reply("Authorization granted.").setEphemeral(true).queue()
        Global.data.userData.addUser(player, userId)
        Global.game.foreach(_.sendAddonMessageToWow(ChatEvents.CHAT_MSG_GUILD, s"""DAUTH::{success=true, player="$player", userId="$userId"}""", "RaidCal"))
        logger.info(s"Sending DAUTH(true, $player, $userId")
      case "deny" =>
        event.reply("Authorization denied.").setEphemeral(true).queue()
        Global.game.foreach(_.sendAddonMessageToWow(ChatEvents.CHAT_MSG_GUILD, s"""DAUTH::{success=false, player="$player", userId="$userId"}""", "RaidCal"))
        logger.info(s"Sending DAUTH(false, $player, $userId")
      case _ =>
        event.reply("Unknown button.").setEphemeral(true).queue()
    }
  }

  override def onStatusChange(event: StatusChangeEvent): Unit = {
    event.getNewStatus match {
      case Status.CONNECTED =>
        isConnected = true
        lastStatus.foreach(game => changeStatus(game.getType, game.getName))

        if (firstConnect) {
          discordConnectionCallback.connected
          firstConnect = false
        } else {
          discordConnectionCallback.reconnected
        }
      case Status.DISCONNECTED =>
        isConnected = false
        discordConnectionCallback.disconnected
      case _ =>
    }
  }

  override def onShutdown(event: ShutdownEvent): Unit = {
    event.getCloseCode match {
      case CloseCode.DISALLOWED_INTENTS =>
        logger.error("Per new Discord rules, you must check the PRESENCE INTENT, SERVER MEMBERS INTENT, and MESSAGE CONTENT INTENT boxes under \"Privileged Gateway Intents\" for this bot in the developer portal. You can find more info at https://discord.com/developers/docs/topics/gateway#privileged-intents")
      case _ =>
    }
  }
}
