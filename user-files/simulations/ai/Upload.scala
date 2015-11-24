package ai

import io.gatling.core.Predef._
import io.gatling.core.session.Expression
import io.gatling.core.structure.ChainBuilder
import io.gatling.http.Predef._
import io.gatling.http.check.HttpCheck

import scala.concurrent.duration._

trait FileUpload extends Simulation with Common {
  
 object FileUpload {

     val filesFeeder = csv("files.csv").queue

    def chain(proxyLabel: String): ChainBuilder = {

      val actionName = proxyLabel + "UA-6_"
        
      feed(filesFeeder)  
      .exec(http(actionName + "upload")
        .post("http://52.11.207.166:9292/upload")
        .formUpload("file","${file}"))     
    }
    
  } 

}
