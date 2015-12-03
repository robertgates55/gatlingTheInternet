package ai

import io.gatling.core.Predef._
import io.gatling.core.session.Expression
import io.gatling.core.structure.ChainBuilder
import io.gatling.http.Predef._
import io.gatling.http.check.HttpCheck

import scala.concurrent.duration._

trait Spider extends Simulation with Common {


 object Spider {

    val urlsFeeder = csv("urls.csv").random
    val searchTermsFeeder = csv("search_terms.csv").random

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

    def extractHttpOrHttps(): HttpCheck = {
     currentLocation
     .transform(s => s.split("://")(0))
     .saveAs("httpOrHttps")
    }

    def getCheckPause(url : String): ChainBuilder = {
      getCheckPause(url,url)
    }

    def getCheckPause(actionName: String, url : String): ChainBuilder = {
      exec(http(actionName).get(url)
       .check(extractLink)
       .check(extractHttpOrHttps)
       .check(extractFormAction))
       .pause(1 seconds, 10 seconds)
    }

    def chain(): ChainBuilder = {

      feed(urlsFeeder)
      .feed(searchTermsFeeder)
      .exec(getCheckPause("http://${url}/"))

      //If there's a form on the homepage, let's try and submit it (50% of the time)
      .doIf(s => s("formAction").asOption[Any].isDefined && util.Random.nextInt(10).toInt > 5) {
        exec(http("http://${url}/").get("http://${url}/").check(extractLink).check(extractFormInputName).check(extractHttpOrHttps))
        .doIf(s => s("formInputName").asOption[Any].isDefined) {
        exec(s => s.set("term", s("term").as[String].replace(" ", "%20")))
         .exec(s => s.set("baseAction",
               s("formAction").asOption[String].getOrElse("")+"?"+
               s("formInputName").asOption[String].getOrElse("")+"="+
               s("term").asOption[String].getOrElse("")))
          .doIfOrElse(s => s("baseAction").as[String].startsWith("http")){
              exec(session => session.set("requestToMake", session("baseAction").asOption[String].getOrElse("")))
          }{
               exec(s => s.set("requestToMake",
                s("httpOrHttps").asOption[String].getOrElse("") + "://" + s("url").asOption[String].getOrElse("") + s("baseAction").asOption[String].getOrElse("")))
          }
          .exec(getCheckPause("form Submit", "${requestToMake}"))
        }
      }

      //Spider 0-10 times - either from the results of the above, or from homepage
      .doIf(session => session("link").asOption[Any].isDefined) {
        exitBlockOnFail{
          repeat(util.Random.nextInt(10).toInt) {
            doIfOrElse(session => session("link").as[String].matches( "https?://.*")) {
              // link = http(s)://something
              getCheckPause("${link}")
            } {
              // else
              doIfOrElse(session => session("link").as[String].startsWith("/")) {
                // link starts with / or //
                doIfOrElse(session => session("link").as[String].startsWith("//")) {
                  // link = //something
                  getCheckPause("${httpOrHttps}:${link}")
                } {
                  // else presume link = /something
                  getCheckPause("${httpOrHttps}://${url}${link}")
                }
              } {
                // else link = not /, //, http or https...
                getCheckPause("${httpOrHttps}://${url}/${link}")
              }
            }
          }
        }
      }

    }

  }







}
