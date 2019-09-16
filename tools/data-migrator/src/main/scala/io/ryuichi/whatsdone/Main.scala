package io.ryuichi.whatsdone

import java.util.concurrent.CompletableFuture

import cats.effect.{ExitCode, IO, IOApp}
import software.amazon.awssdk.services.dynamodb.DynamoDbAsyncClient
import software.amazon.awssdk.services.dynamodb.model.{AttributeValue, ScanRequest}

import scala.jdk.CollectionConverters._

object Main extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
    for {
      items <- DynamoTest.dumpItems(args(0))
      _ <- IO(println(items))
    } yield ExitCode.Success
  }
}

object DynamoTest {
  def dumpItems(tableName: String): IO[List[Map[String, AttributeValue]]] = {
    val client = DynamoDbAsyncClient.create
    val response = client.scan(ScanRequest.builder.tableName(tableName).build())
    val items: CompletableFuture[List[Map[String, AttributeValue]]] = response.thenApply(_.items().asScala.toList.map(_.asScala.toMap))
    IO(items.join())
  }
}
