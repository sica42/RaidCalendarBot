package raidcalbot.discord

import net.dv8tion.jda.api.JDA
import net.dv8tion.jda.api.entities.channel.concrete.TextChannel
import raidcalbot.common.{Global, RaidCalBotConfig, WowExpansion}

import scala.collection.JavaConverters._
import scala.collection.mutable

object MessageResolver {
  def apply(jda: JDA): MessageResolver = new MessageResolver(jda)
}

class MessageResolver(jda: JDA) {

  protected val linkRegexes = Seq(
    "item" -> "\\|.+?\\|Hitem:(\\d+):.+?\\|h\\[(.+?)]\\|h\\|r".r,
    "spell" -> "\\|.+?\\|(?:Hspell|Henchant)?:(\\d+).*?\\|h\\[(.+?)]\\|h\\|r".r,
    "quest" -> "\\|.+?\\|Hquest:(\\d+):.+?\\|h\\[(.+?)]\\|h\\|r".r
  )

  protected val linkSite = "http://database.turtle-wow.org"


  def stripColorCoding(message: String): String = {
    val hex = "\\|c[0-9a-fA-F]{8}"
    val pass1 = s"$hex(.*?)\\|r".r
    val pass2 = hex.r

    pass2.replaceAllIn(pass1.replaceAllIn(message.replace("$", "\\$"), _.group(1)), "")
  }

  def resolveEmojis(message: String): String = {
    val regex = "(?<=:).*?(?=:)".r

    // could do some caching here later
    val emojiMap = jda.getEmojis.asScala.map(emote => {
      emote.getName.toLowerCase -> emote.getId
    }).toMap

    val alreadyResolved = mutable.Set.empty[String]
    regex.findAllIn(message).foldLeft(message) {
      case (result, possibleEmoji) =>
        val lPossibleEmoji = possibleEmoji.toLowerCase
        if (alreadyResolved(lPossibleEmoji)) {
          result
        } else {
          emojiMap.get(lPossibleEmoji).fold(result)(id => {
            alreadyResolved += lPossibleEmoji
            result.replace(s":$possibleEmoji:", s"<:$possibleEmoji:$id>")
          })
        }
    }
  }
}
