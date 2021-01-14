package io.ryuichi.whatsdone

import io.gatling.core.Predef._
import io.gatling.http.Predef._
import io.ryuichi.whatsdone.helper.{CognitoResource, TokenProvider, UserProvider}
import software.amazon.awssdk.regions.Region

class BasicSimulation extends Simulation {
  private val REQUEST_NAME = "Get Dones"

  private val userProvider = new UserProvider("/whatsdone/ci/e2e-test")
  private val cognitoResource = CognitoResource("s6onvlog4hqsbcbmgmtthterd", Region.AP_SOUTHEAST_2)
  private val token = new TokenProvider(cognitoResource).token(userProvider.testUser)

  private val httpConf = http
    .baseURL("https://whatsdone-ci-api.ryuichi.io")
    .acceptHeader("application/json")
    .header(HttpHeaderNames.Authorization, token.authHeader)
    .doNotTrackHeader("1")
    .acceptLanguageHeader("en-US,en;q=0.5")
    .acceptEncodingHeader("gzip, deflate")

  private val scn = scenario(this.getClass.getSimpleName)
    .exec(http(REQUEST_NAME).get("/dones"))
    .pause(5)

  setUp(
    scn.inject(atOnceUsers(100)).protocols(httpConf)
  ).assertions(
    details(REQUEST_NAME).responseTime.mean.lt(15000),
    details(REQUEST_NAME).allRequests.percent.is(100),
    details(REQUEST_NAME).failedRequests.percent.is(0)
  )
}
