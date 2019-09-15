package io.ryuichi.whatsdone

import java.util.concurrent.CompletableFuture

import cats.instances.string._
import cats.syntax.semigroup._
import software.amazon.awssdk.services.dynamodb.DynamoDbAsyncClient
import software.amazon.awssdk.services.dynamodb.model.{AttributeValue, ScanRequest}

import scala.jdk.CollectionConverters._

object Main extends App {
  println("Hello " |+| "Cats!")
  DynamoTest.dumpItems(args(0))
}

object DynamoTest {
  def dumpItems(tableName: String): Unit = {
    val client = DynamoDbAsyncClient.create
    val response = client.scan(ScanRequest.builder.tableName(tableName).build())
    val items: CompletableFuture[List[Map[String, AttributeValue]]] = response.thenApply(_.items().asScala.toList.map(_.asScala.toMap))
    items.whenComplete((items, err) => {
      try {
        if (items != null) {
          items.foreach(println)
        } else {
          err.printStackTrace()
        }
      } finally {
        client.close()
      }
    })

    items.join()
  }
}
