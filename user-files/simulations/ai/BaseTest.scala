package ai

import io.gatling.core.Predef._
import io.gatling.core.session.Expression
import io.gatling.core.structure.ChainBuilder
import io.gatling.http.Predef._
import io.gatling.http.check.HttpCheck

import scala.concurrent.duration._

class BaseTest extends Simulation with Spider with GoogleThenSpider with FileDownload with FileUpload with Polling with PlanJourney{

  object BaseTest {
    def chooser(actionName: String): ChainBuilder = {
      randomSwitch( // beware: use parentheses, not curly braces!
       55.0 -> (Spider.chain()),
       10.0 -> (PlanJourney.chain()),
       10.0 -> (GoogleThenSpider.chain()),
       10.0 -> (FileDownload.chain()),
       10.0 -> (FileUpload.chain()),
       5.0 -> (Polling.chain())
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
    // If you need to run via a proxy
    // .proxy(Proxy("proxy.somewhere.net", 8080).httpsPort(443))


  // test run config
  val testDuration = 3 minutes
  val maxUserCount = 100
  val meanUserLife = 600 // seconds
  val numberToInjectOverDuration = ((math.ceil(testDuration.toSeconds) * maxUserCount) / meanUserLife).toInt //hoping test duration is in seconds

  val standardUsers = scenario("Standard User").exec(BaseTest.chooser(""))

  setUp(
    standardUsers
      .inject(
        rampUsers(math.ceil(numberToInjectOverDuration).toInt) over testDuration)
      .protocols(httpNoProxyConf)
   ).maxDuration(testDuration)



}
