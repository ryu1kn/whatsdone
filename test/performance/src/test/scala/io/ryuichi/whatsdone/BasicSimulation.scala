package io.ryuichi.whatsdone

import io.gatling.core.Predef._
import io.gatling.http.Predef._

class BasicSimulation extends Simulation {

  val COOKIE = "connect.sid=ENCODED_SESSION_ID"

  val httpConf = http
    .baseURL("https://whatsdone-api.ryuichi.io")
    .acceptHeader("application/json")
    .header(HttpHeaderNames.Cookie, COOKIE)
    .doNotTrackHeader("1")
    .acceptLanguageHeader("en-US,en;q=0.5")
    .acceptEncodingHeader("gzip, deflate")

  val scn = scenario("BasicSimulation")
    .exec(http("Get Dones").get("/dones"))
    .pause(5)

  setUp(scn.inject(atOnceUsers(100)))
    .protocols(httpConf)

}
