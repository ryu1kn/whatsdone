package io.ryuichi.whatsdone.helper

import software.amazon.awssdk.regions.Region
import software.amazon.awssdk.services.cognitoidentityprovider.CognitoIdentityProviderClient
import software.amazon.awssdk.services.cognitoidentityprovider.model.{AuthFlowType, AuthenticationResultType, InitiateAuthRequest}

import scala.jdk.CollectionConverters.MapHasAsJava

class TokenProvider(cognito: CognitoResource) {
  private val client = CognitoIdentityProviderClient.builder().region(cognito.region).build()

  def token(user: LoginInfo): Token =
    Token(client.initiateAuth(makeAuthRequest(user)).authenticationResult())

  private def makeAuthRequest(user: LoginInfo) = InitiateAuthRequest.builder()
    .authFlow(AuthFlowType.USER_PASSWORD_AUTH)
    .clientId(cognito.clientId)
    .authParameters(Map("USERNAME" -> user.username, "PASSWORD" -> user.password).asJava)
    .build()
}

case class CognitoResource(clientId: String, region: Region)

case class Token(authResult: AuthenticationResultType) {
  val authHeader = s"${authResult.tokenType()} ${authResult.idToken()}"
}
