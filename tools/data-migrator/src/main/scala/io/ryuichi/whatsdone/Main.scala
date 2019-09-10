package io.ryuichi.whatsdone

import java.util.concurrent.CompletableFuture

import cats.instances.string._
import cats.syntax.semigroup._
import software.amazon.awssdk.services.dynamodb.DynamoDbAsyncClient
import software.amazon.awssdk.services.dynamodb.model.ListTablesRequest

import scala.jdk.CollectionConverters._

object Main extends App {
  println("Hello " |+| "Cats!")
  DynamoTest.listTables()
}

object DynamoTest {
  def listTables(): Unit = {
    val client = DynamoDbAsyncClient.create
    val response = client.listTables(ListTablesRequest.builder.build)
    val tableNames: CompletableFuture[List[String]] = response.thenApply(_.tableNames().asScala.toList)
    tableNames.whenComplete((tables, err) => {
      try {
        if (tables != null) {
          tables.foreach(println)
        } else {
          err.printStackTrace()
        }
      } finally {
        client.close()
      }
    })

    tableNames.join()
  }
}
