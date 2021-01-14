package io.ryuichi.whatsdone.helper

import software.amazon.awssdk.regions.Region
import software.amazon.awssdk.services.secretsmanager.SecretsManagerClient
import software.amazon.awssdk.services.secretsmanager.model.GetSecretValueRequest

class UserProvider(secretId: String) {
  private val secretsClient = SecretsManagerClient.builder().region(Region.AP_SOUTHEAST_2).build()

  def testUser: LoginInfo = {
    val secret = secretsClient.getSecretValue(makeRequest()).secretString()
    LoginInfo(
      extractValue(secret, "username"),
      extractValue(secret, "password")
    )
  }

  private def makeRequest() = GetSecretValueRequest.builder().secretId(secretId).build()

  private def extractValue(json: String, key: String) = s""""$key":"(.*?)"""".r.findFirstMatchIn(json).get.group(1)
}

case class LoginInfo(username: String, password: String)
