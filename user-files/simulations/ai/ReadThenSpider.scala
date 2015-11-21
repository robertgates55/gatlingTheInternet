package ai

import io.gatling.core.Predef._
import io.gatling.core.session.Expression
import io.gatling.core.structure.ChainBuilder
import io.gatling.http.Predef._
import io.gatling.http.check.HttpCheck

import scala.concurrent.duration._

class ReadThenSpider extends Simulation {

  object Spider {

    val urlsFeeder = csv("urlsUS.csv").random
    val searchTermsFeeder = csv("search_terms.csv").random

    val noProxy = chain("[D]_UA-1_http")
    val withProxy = chain("[P]_UA-1_http")

    def extractLink(): HttpCheck = {
      css("a","href")
      //  regex( """<a [^>]*href="([^#"]+)""")
          .findAll
          .transform(s => util.Random.shuffle(s).head)
          .optional
          .saveAs("link")
    }


    def extractPostAction(): HttpCheck = {
     css("form[method='post']","action")
          .find
          .optional
          .saveAs("postAction")
    }

    def extractFormAction(): HttpCheck = { 
     css("form[method='get']","action")
          .find
          .optional
          .saveAs("formAction")
    }

    def extractFormInputName(): HttpCheck = {
     css("form[action='${formAction}'] input[type='text']","name")
          .find
          .transformOption(extract => extract.orElse(Some("")))
          .optional
          .saveAs("formInputName")
     
    }

    def extractBaseUrl(): HttpCheck = {
     currentLocation.saveAs("baseUrl")
    }

    def getCheckPause(actionName: String, url : String): ChainBuilder = {
      exec(http(actionName).get(url)
       .check(extractLink)
       .check(extractFormAction))
       .pause(1 seconds, 10 seconds)
    }

    def chain(actionName: String): ChainBuilder = {
      feed(urlsFeeder)
      .feed(searchTermsFeeder)
        .exec(getCheckPause(actionName, "http://${url}/")) 
        
        //If there's a form on the homepage, let's try and submit it (50% of the time)
        .doIf(session => session("formAction").asOption[Any].isDefined && util.Random.nextInt(10).toInt > 5) {
         exec(http(actionName).get("http://${url}/").check(extractLink).check(extractFormInputName).check(extractBaseUrl)) 
         .doIf(session => session("formInputName").asOption[Any].isDefined) {
           exec(s => s.set("term", s("term").as[String].replace(" ", "%20")))
           .exec(session => session.set("baseAction",
                session("formAction").asOption[String].getOrElse("")+"?"+
                session("formInputName").asOption[String].getOrElse("")+"="+
                session("term").asOption[String].getOrElse("")))
           .doIfOrElse(session => session("baseAction").as[String].startsWith("http")){
               exec(session => session.set("requestToMake", session("baseAction").asOption[String].getOrElse("")))
           }{
               exec(session => session.set("requestToMake",
                 session("baseUrl").asOption[String].getOrElse("")+session("baseAction").asOption[String].getOrElse("")))     
           }
           .exec(getCheckPause(actionName,"${requestToMake}"))   
         }
        }
        
        //Spider 0-10 times - either from the results of the above, or from homepage
        .doIf(session => session("link").asOption[Any].isDefined) {
          repeat(util.Random.nextInt(10).toInt) {
            doIfOrElse(session => session("link").as[String].matches( "https?://.*")) {
              // link = http(s)://something
              getCheckPause(actionName, "${link}")
            } {
              // else
              doIfOrElse(session => session("link").as[String].startsWith("/")) {
                // link starts with / or //
                doIfOrElse(session => session("link").as[String].startsWith("//")) {
                  // link = //something
                  getCheckPause(actionName, "http:${link}")
                } {
                  // else presume link = /something
                  getCheckPause(actionName, "http://${url}${link}")
                }
              } {
                // else link = not /, //, http or https...
                getCheckPause(actionName, "http://${url}/${link}")
              }
            }
          }
        }
        
    }
} 

  def randomUA: Expression[String] = {
    s => UAs.userAgents.get(util.Random.nextInt(UAs.userAgents.length))
  }

  val httpNoProxyConf = http
    .acceptHeader("text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8")
    .doNotTrackHeader("1")
    .acceptLanguageHeader("en-US,en;q=0.5")
    .acceptEncodingHeader("gzip, deflate")
    .userAgentHeader(randomUA)
    .inferHtmlResources
    .silentResources
    .disableCaching
    .disableAutoReferer
    .maxConnectionsPerHostLikeChrome
    .maxRedirects(5)

  val httpWithProxyConf = httpNoProxyConf
  .proxy(Proxy("proxy.qa.streamshield.net", 3128).httpsPort(3128))

  val usersNoProxy = scenario("Users No proxy").exec(Spider.noProxy)
  val usersWithProxy = scenario("Users with proxy").exec(Spider.withProxy)

  // test run config
  val testDuration = 3 minutes
  val maxUserCount = 10
  val meanUserLife = 600 // seconds
  val numberToInjectOverDuration = ((math.ceil(testDuration.toSeconds) * maxUserCount) / meanUserLife).toInt //hoping test duration is in seconds

  setUp(
    //usersWithProxy
    //  .inject(
    //    rampUsers(math.ceil(numberToInjectOverDuration).toInt) over testDuration)
    //  .protocols(httpWithProxyConf)
    //,
     usersNoProxy
       .inject(
         rampUsers(math.ceil(numberToInjectOverDuration * 1.05).toInt) over testDuration)
       .protocols(httpNoProxyConf)
  ).maxDuration(testDuration)


}
