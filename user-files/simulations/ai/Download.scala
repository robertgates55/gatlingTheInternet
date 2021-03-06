package ai

import io.gatling.core.Predef._
import io.gatling.core.session.Expression
import io.gatling.core.structure.ChainBuilder
import io.gatling.http.Predef._
import io.gatling.http.check.HttpCheck

import scala.concurrent.duration._

trait FileDownload extends Simulation with Common {

 object FileDownload {

    def extractDownloadLink(): HttpCheck = {
      css("div.example a","href")
          .findAll
          .transform(s => util.Random.shuffle(s).head)
          .optional
          .saveAs("downloadLink")
    }

    def getCheckPause(actionName: String, url : String): ChainBuilder = {
      exec(http(actionName).get(url)
       .check(extractDownloadLink))
       .pause(1 seconds, 10 seconds)
    }

    def chain(): ChainBuilder = {

      exec(getCheckPause("browse", "http://the-internet.herokuapp.com/download"))

      //If there's a file to download, download it
      .doIf(session => session("downloadLink").asOption[Any].isDefined) {
        exec(http("download").get("http://the-internet.herokuapp.com/${downloadLink}"))
      }

    }

  }

}
