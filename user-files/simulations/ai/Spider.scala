package ai

import io.gatling.core.Predef._
import io.gatling.core.session.Expression
import io.gatling.core.structure.ChainBuilder
import io.gatling.http.Predef._
import io.gatling.http.check.HttpCheck

import scala.concurrent.duration._

trait Spider extends Simulation with Common {

  
 object Spider {

    val urlsFeeder = csv("urlsUS.csv").random
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

    def extractBaseUrl(): HttpCheck = {
     currentLocation.saveAs("baseUrl")
    }

    def getCheckPause(actionName: String, url : String): ChainBuilder = {
      exec(http(actionName).get(url)
       .check(extractLink)
       .check(extractFormAction))
       .pause(1 seconds, 10 seconds)
    }

    def chain(proxyLabel: String): ChainBuilder = {

      val actionName = proxyLabel + "UA-1/2_"

      feed(urlsFeeder)
      .feed(searchTermsFeeder)
      .exec(getCheckPause(actionName + "browse", "http://${url}/")) 
        
      //If there's a form on the homepage, let's try and submit it (50% of the time)
      .doIf(session => session("formAction").asOption[Any].isDefined && util.Random.nextInt(10).toInt > 5) {
        exec(http(actionName + "browse").get("http://${url}/").check(extractLink).check(extractFormInputName).check(extractBaseUrl)) 
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
          .exec(getCheckPause(actionName + "browse","${requestToMake}"))   
        }
      }
        
      //Spider 0-10 times - either from the results of the above, or from homepage
      .doIf(session => session("link").asOption[Any].isDefined) {
        repeat(util.Random.nextInt(10).toInt) {
          doIfOrElse(session => session("link").as[String].matches( "https?://.*")) {
            // link = http(s)://something
            getCheckPause(actionName + "browse", "${link}")
          } {
            // else
            doIfOrElse(session => session("link").as[String].startsWith("/")) {
              // link starts with / or //
              doIfOrElse(session => session("link").as[String].startsWith("//")) {
                // link = //something
                getCheckPause(actionName + "browse", "http:${link}")
              } {
                // else presume link = /something
                getCheckPause(actionName + "browse", "http://${url}${link}")
              }
            } {
              // else link = not /, //, http or https...
              getCheckPause(actionName + "browse", "http://${url}/${link}")
            }
          }
        }
      }

    }
    
  } 







}