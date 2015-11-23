package ai

import io.gatling.core.Predef._
import io.gatling.core.session.Expression
import io.gatling.core.structure.ChainBuilder
import io.gatling.http.Predef._
import io.gatling.http.check.HttpCheck

import scala.concurrent.duration._

class BaseTest extends Simulation with Spider with Google {

  object BaseTest {
    def chooser(actionName: String): ChainBuilder = {
      randomSwitch( // beware: use parentheses, not curly braces!
       90.0 -> (Spider.chain(actionName)),
       10.0 -> (GoogleSpider.chain(actionName))
      )
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


    // test run config
  val testDuration = 3 minutes
  val maxUserCount = 100
  val meanUserLife = 600 // seconds
  val numberToInjectOverDuration = ((math.ceil(testDuration.toSeconds) * maxUserCount) / meanUserLife).toInt //hoping test duration is in seconds

  val usersNoProxy = scenario("Users No proxy").exec(BaseTest.chooser("[D]_"))
  val usersWithProxy = scenario("Users with proxy").exec(BaseTest.chooser("[P]_"))

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