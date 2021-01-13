package io.ryuichi.whatsdone.helper

import software.amazon.awssdk.regions.Region
import software.amazon.awssdk.services.secretsmanager.SecretsManagerClient
import software.amazon.awssdk.services.secretsmanager.model.GetSecretValueRequest

object UserProvider {
  private val secretsClient = SecretsManagerClient.builder().region(Region.AP_SOUTHEAST_2).build()

  def testUser: LoginInfo = {
    val secretValueRequest = GetSecretValueRequest.builder().secretId("/whatsdone/ci/e2e-test").build()
    val secret = secretsClient.getSecretValue(secretValueRequest).secretString()
    val username = """"username":"(.*?)"""".r.findFirstMatchIn(secret).get.group(1)
    val password = """"password":"(.*?)"""".r.findFirstMatchIn(secret).get.group(1)
    LoginInfo(username, password)
  }
}

case class LoginInfo(username: String, password: String)
