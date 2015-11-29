package ai

import io.gatling.core.Predef._
import io.gatling.core.session.Expression
import io.gatling.core.structure.ChainBuilder
import io.gatling.http.Predef._
import io.gatling.http.check.HttpCheck

import scala.concurrent.duration._

trait PlanJourney extends Simulation with Common {


 object PlanJourney {

   val from = csv("fromStations.csv").random
   val to = csv("toStations.csv").random

   def extractHttpOrHttps(): HttpCheck = {
     currentLocation
     .transform(s => s.split("://")(0))
     .saveAs("httpOrHttps")
    }

    def getCheckPause(url: String): ChainBuilder = {
      getCheckPause(url,url)
    }

    def getCheckPause(actionName: String, url : String): ChainBuilder = {
      exec(http(actionName).get(url)
       .check(extractLink)
       .check(extractHttpOrHttps))
       .pause(1 seconds, 10 seconds)
    }

    def chain(): ChainBuilder = {

      feed(from)
      .feed(to)

      .exec(http("Plan Journey")
        .get("https://tfl.gov.uk/plan-a-journey/results")
          .queryParam("IsAsync","true")
          .queryParam("JpType","publictransport")
          .queryParam("InputFrom","${from}")
          .queryParam("From","${from}")
          //.queryParam("FromId","1000180")
          .queryParam("PreviousFrom","")
          .queryParam("InputTo","${to}")
          //.queryParam("To","Oxford Circus Underground Station")
          .queryParam("Mode","bus")
          .queryParam("Mode","tube")
          .queryParam("Mode","national-rail")
          .queryParam("Mode","dlr")
          .queryParam("Mode","overground")
          .queryParam("Mode","tflrail")
          .queryParam("Mode","river-bus")
          .queryParam("Mode","tram")
          .queryParam("Mode","cable-car")
          .queryParam("Mode","coach")
          .queryParam("CyclePreference","AllTheWay")
          .queryParam("WalkingSpeedWalking","average")
          .queryParam("JourneyPreference","leasttime")
          .queryParam("AccessibilityPreference","norequirements")
          .queryParam("MaxWalkingMinutes","40")
          .queryParam("WalkingSpeedTransport","average")
          .queryParam("InputVia","")
          .queryParam("DataSetsJson","[[\"stopPoints\",\"journeyPlannerNoSubmit?Input={{input}}\"]]")
          .queryParam("Modes","tube,dlr,overground,tflrail,bus,river-bus,tram,cable-car,national-rail")
          .queryParam("NationalSearch","false")
          .queryParam("SavePreferences","false")
          .queryParam("IsMultipleJourneySelection","False")
          .queryParam("IsPastWarning","False")
          .check(extractLink)
          .check(extractHttpOrHttps)
      )

    }

  }

}