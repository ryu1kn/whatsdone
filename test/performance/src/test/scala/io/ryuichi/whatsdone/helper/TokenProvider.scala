package io.ryuichi.whatsdone.helper

import software.amazon.awssdk.regions.Region
import software.amazon.awssdk.services.cognitoidentityprovider.CognitoIdentityProviderClient
import software.amazon.awssdk.services.cognitoidentityprovider.model.{AuthFlowType, AuthenticationResultType, InitiateAuthRequest}

import scala.collection.JavaConverters.mapAsJavaMap

object TokenProvider {
  private val client = CognitoIdentityProviderClient.builder().region(Region.AP_SOUTHEAST_2).build()
  private val clientId = "s6onvlog4hqsbcbmgmtthterd"

  def token(user: LoginInfo): Token = {
    val initiateAuthRequest = InitiateAuthRequest.builder()
      .authFlow(AuthFlowType.USER_PASSWORD_AUTH)
      .clientId(clientId)
      .authParameters(mapAsJavaMap(Map("USERNAME" -> user.username, "PASSWORD" -> user.password)))
      .build()
    val result = client.initiateAuth(initiateAuthRequest).authenticationResult()
    Token(result)
  }
}

case class Token(authResult: AuthenticationResultType) {
  val authHeader = s"${authResult.tokenType()} ${authResult.idToken()}"
}
