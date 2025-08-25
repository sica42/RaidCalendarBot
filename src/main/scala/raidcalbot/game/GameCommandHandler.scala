package raidcalbot.game

trait GameCommandHandler {

  def sendMessageToWow(tp: Byte, message: String, target: Option[String])
  def sendAddonMessageToWow(tp: Byte, message: String, prefix: String)
  def sendNotification(message: String)
}
