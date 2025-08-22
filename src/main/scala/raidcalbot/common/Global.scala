package raidcalbot.common

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

import io.netty.channel.EventLoopGroup
import raidcalbot.discord.Discord
import raidcalbot.game.GameCommandHandler

object Global {

  var group: EventLoopGroup = _
  var config: RaidCalBotConfig = _
  var data: RaidCalBotDataManager = _

  var discord: Discord = _
  var game: Option[GameCommandHandler] = None

  def getTime: String = {
    LocalDateTime.now.format(DateTimeFormatter.ofPattern("HH:mm:ss"))
  }
}
