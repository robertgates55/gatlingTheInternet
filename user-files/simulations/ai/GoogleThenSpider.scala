package ai

import io.gatling.core.Predef._
import io.gatling.core.session.Expression
import io.gatling.core.structure.ChainBuilder
import io.gatling.http.Predef._
import io.gatling.http.check.HttpCheck

import scala.concurrent.duration._

trait GoogleThenSpider extends Simulation with Common {

 object GoogleThenSpider {

    val feeder = csv("search_terms.csv").random

    def extractGoogleLink(): HttpCheck = {
      css("#search a", "href")
        .findAll
        .transform(s => util.Random.shuffle(s).head)
        .saveAs("link")
    }

    def getCheckPause(actionName: String, url: String, httpCheck: HttpCheck = extractLink())
    : ChainBuilder = {
      exec(http(actionName).get(url).check(httpCheck)).pause(5 seconds, 20 seconds)
    }

    def chain(): ChainBuilder = {

      feed(feeder)
        // goto google
        .exec(http("google").get("http://google.co.uk")
        .check(substring(""" name="q" """), substring("""I'm Feeling Lucky""")))
        .pause(2 seconds, 10 seconds)

        // type query and search
        .exec(
          exitBlockOnFail{
            foreach(session => session("term").as[String].toList, "char") {
              exec(session => session.set(
                "currentTerm",
                session("currentTerm").asOption[String].getOrElse("") + session("char").as[String]))
              .exec(session => session.set(
                "currentTermSize",
                session("currentTerm").asOption[String].getOrElse("").length))
                .doIf(session => session("char").as[String] != " ") {
                  randomSwitch(
                    50.0 -> (
                      exec(http("auto-complete")
                        .get(
                          "http://google.co.uk/complete/search?" +
                            "client=hp&hl=en-GB&gs_rn=64&gs_ri=hp&" +
                            "cp=${currentTermSize}&gs_id=6&" +
                            "q=${currentTerm}&xhr=t")
                        .check(status.is(200))
                        //.check(substring("${currentTerm}"))
                      )
                    )
                  )
                }
              .pause(10 milliseconds, 200 milliseconds)
            }
          }
        )
        .pause(2 seconds, 10 seconds)

        // do the final search
        .exec(s => s.set("term", s("term").as[String].replace(" ", "+")))
        .exec(
          getCheckPause(
            actionName = "search",
            url = "http://google.co.uk/search?site=&source=hp&q=${term}&oq=${term}",
            httpCheck = extractGoogleLink()
          )
        )
        // click on links up to 10 'deep'
        .doIf(session => session("link").asOption[Any].isDefined) {
          exitBlockOnFail{
            repeat(util.Random.nextInt(10).toInt) {
              doIfOrElse(session => session("link").as[String].matches( "https?://.*")) {
                // link = http(s)://something
                getCheckPause("browse", "${link}")
              } {
                // else
                doIfOrElse(session => session("link").as[String].startsWith("/")) {
                  // link starts with / or //
                  doIfOrElse(session => session("link").as[String].startsWith("//")) {
                    // link = //something
                    getCheckPause("browse", "http:${link}")
                  } {
                    // else presume link = /something
                    //getCheckPause("browse","http://${url}${link}")
                    exec()
                  }
                } {
                  // else link = not /, //, http or https...
                  //getCheckPause("browse","http://${url}/${link}")
                  exec()
                }
              }
            }
          }
        }
    }
  }









}
