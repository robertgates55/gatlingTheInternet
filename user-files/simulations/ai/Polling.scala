package ai

import io.gatling.core.Predef._
import io.gatling.core.session.Expression
import io.gatling.core.structure.ChainBuilder
import io.gatling.http.Predef._
import io.gatling.http.check.HttpCheck

import scala.concurrent.duration._

trait Polling extends Simulation with Common {

 object Polling {

    def chain(proxyLabel: String): ChainBuilder = {

      val actionName = proxyLabel + "UA-7_"

      repeat(util.Random.nextInt(20).toInt) {
        exec(http(actionName + "poll")
          .get("https://data.flightradar24.com/zones/fcgi/feed.js")
            .queryParam("bounds","54.47391589740373,47.18782995162306,-9.556199999999535,15.84228515625")
            .queryParam("faa","1")
            .queryParam("mlat","1")
            .queryParam("flarm","1")
            .queryParam("adsb","1")
            .queryParam("gnd","1")
            .queryParam("air","1")
            .queryParam("vehicles","1")
            .queryParam("estimated","1")
            .queryParam("maxage","900")
            .queryParam("gliders","1")
            .queryParam("stats","1")
        )
        .pause(5 seconds)
      }
    }
    
  } 

}
