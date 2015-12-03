package ai

import io.gatling.core.Predef._
import io.gatling.core.session.Expression
import io.gatling.core.structure.ChainBuilder
import io.gatling.http.Predef._
import io.gatling.http.check.HttpCheck

import scala.concurrent.duration._

trait FileUpload extends Simulation with Common {

 object FileUpload {

     val filesFeeder = csv("files.csv").random

    def chain(): ChainBuilder = {

      feed(filesFeeder)
      .exec(http("upload")
        .post("http://the-internet.herokuapp.com/upload")
        .formUpload("file","${file}"))
    }

  }

}
