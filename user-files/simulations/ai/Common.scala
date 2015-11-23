package ai

import io.gatling.core.Predef._
import io.gatling.core.session.Expression
import io.gatling.core.structure.ChainBuilder
import io.gatling.http.Predef._
import io.gatling.http.check.HttpCheck

import scala.concurrent.duration._

trait Common {

     def extractLink(): HttpCheck = {
      css("a","href")
      //  regex( """<a [^>]*href="([^#"]+)""")
          .findAll
          .transform(s => util.Random.shuffle(s).head)
          .optional
          .saveAs("link")
    }



}