package io.ryuichi.whatsdone

import io.gatling.core.Predef._
import io.gatling.http.Predef._
import io.ryuichi.whatsdone.helper.{TokenProvider, UserProvider}

class BasicSimulation extends Simulation {
  val REQUEST_NAME = "Get Dones"
  private val token = TokenProvider.token(UserProvider.testUser)

  val httpConf = http
    .baseURL("https://whatsdone-ci-api.ryuichi.io")
    .acceptHeader("application/json")
    .header(HttpHeaderNames.Authorization, token.authHeader)
    .doNotTrackHeader("1")
    .acceptLanguageHeader("en-US,en;q=0.5")
    .acceptEncodingHeader("gzip, deflate")

  val scn = scenario("BasicSimulation")
    .exec(http(REQUEST_NAME).get("/dones"))
    .pause(5)

  setUp(
    scn.inject(atOnceUsers(100)).protocols(httpConf)
  ).assertions(
    details(REQUEST_NAME).responseTime.mean.lt(2000),
    details(REQUEST_NAME).allRequests.percent.is(100),
    details(REQUEST_NAME).failedRequests.percent.is(0)
  )
}
