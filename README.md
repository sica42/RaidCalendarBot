RaidCalendarBot -- README
=================

RaidCalendarBot acts as a command central for [RaidCalendar](https://github.com/sica42/RaidCalendar) addon developed for Turtle WoW.
It integrates with the following services:
* Discord bot, for user identification and authorization
* raid-helper.dev for raid signups
* raidres.fly.dev for soft reserving items

The bot also have the ability to do price check on items (prices from https://www.wowauctions.net/). This is done by whispering the bot #PC#item link. You will need to level the bot to lvl 10 for it to be able to whisper a reply!

yadda yadda, update this!

## How it works
The bot uses Discord's API to login to your Discord server. It then uses supplied information to login as a WoW character onto your chosen server. Once it logs in to WoW as a character, it will listen for commands from [RaidCalendar](https://github.com/sica42/RaidCalendar) on the guild addon channel.

##### DO NOT, under any circumstances, use this bot on an account with existing characters!
Even though this bot does not do anything malicious, some servers may not like a bot connecting, and GMs may ban the account!

## Setup & Prerequisites

1. First you will want to create a Discord Bot on your discord account:
   * Go to https://discordapp.com/developers/applications/
   * Sign into your Discord account if necessary and click "Create an application"
   * Change the application name to something meaningful like "RaidCalendar"
   * On the left click the Bot tab
   * Add a Bot
   * Uncheck Public Bot option
   * **Check PRESENCE INTENT, SERVER MEMBERS INTENT and MESSAGE CONTENT INTENT under "Privileged Gateway Intents"** This is important! Without it, your bot will not work!
   * Under token click Copy. This is the value RaidCalendar will use to login to Discord.

2. Create a Discord account to use with raidres.fly.dev.
   * This account will be used for all soft reserves. It is recommended that you create a new account for this.
   * After creating the account, go to raidres.fly.dev and sign in.
   * You then need to find and copy the `jwt` cookie for use in the `raidcalbot.conf`.
     * Press F12 to open developer tools.
     * Chrome: Go to `Application` tab and in the left menu, then find and open `Storage->Cookies->https://raidres.fly.dev`.
     * Firefox: Go `Storage` tab and in the left menu, then find and open `Cookies->https://raidres.fly.dev`. 
     * You should see a cookie named `jwt`. Double click the value field and copy the value.

3. Configure WoW Chat by opening `raidcalbot.conf` in a text editor.
   * You can also create your own file, using the supplied `raidcalbot.conf` as a template.
   * In section **discord**:
     * **token**: Paste the above copied Bot token, or set the DISCORD_TOKEN environment variable.
   * In section **raidhelper**:
     * **api_key**: use /apikey on your Discord server to show the API key (you need admin access).
     * **server_id**: Discord server ID.
   * In section **raidres**:
     * **cookies**: Paste the above copied value so that it reads cookies="jwt=your cookie value"
   * In section **wow**:
     * **platform**: Leave as **Mac** unless your target server has Warden (anticheat) disabled AND it is blocking/has disabled Mac logins. In this case put **Windows**.
     * **version**: put either 1.12.1, 2.4.3, 3.3.5, 4.3.4, or 5.4.8 based on the server's expansion.
     * **build**: you can include a build=<build number> setting in the config, if you are using a custom build version on your server. Optionally you can also use **realm_build** and **game_build** options if the number used is different for each server.
     * **realmlist**: this is server's realmlist, same as in your realmlist.wtf file.
     Example values are logon.turtle-wow.org or cnlogon.turtle-wow.org
     * **realm**: This is the realm name the Bot will connect to.
     It is the Text shown on top of character list window. Put ONLY the name, do NOT put the realm type like PVP or PVE.
     In the following example, the **realm** value is The Construct
     * ![realm-construct](https://raw.githubusercontent.com/fjaros/raidcalbot/master/images/example3.png)
     * **account**: The bot's WoW game account, or set the WOW_ACCOUNT environment variable.
     * **password**: The bot's WoW game account password, or set the WOW_PASSWORD environment variable.
     * **character**: Your character's name as would be shown in the character list, or set the WOW_CHARACTER environment variable.

4. Invite your bot to Discord
   * Go back to https://discordapp.com/developers/applications/ and click your new Bot application.
   * In browser enter: https://discordapp.com/oauth2/authorize?client_id=CLIENT_ID&scope=bot
     * Replace **CLIENT_ID** with the value from Discord applications page.

## Run from source
1. WoW Chat is written in Scala and compiles to a Java executable using [maven](https://maven.apache.org).
2. It uses Java JDK 21 and Scala 2.12.20.
3. Run `mvn clean package` which will produce a file in the target folder called `raidcalendar-1.1.1.zip`
4. unzip `raidcalendar-1.1.1.zip`, edit the configuration file and run `java -jar raidcalendar.jar <config file>`
   * If no config file is supplied, the bot will try to use `raidcalendar.conf`

## Run from JAR file
1. Download the latest release from https://github.com/sica42/RaidCalendarBot/releases/latest
2. It requires Java 21 to run
2. Unzip `raidcalendar-1.1.1.zip`, edit the configuration file and run with `run.bat` or `run.sh`
