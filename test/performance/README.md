
# What's Done Load Test

## Prerequisite

* Docker

## Running test

* With Docker

  ```sh
  $ docker-compose -f docker-compose.yaml run test
  ```

  on macOS using host machine cache (c.f. [How to avoid re-downloading the sbt dependency package when moving the sbt project to a new machine][1])

  ```sh
  $ docker-compose -f docker-compose.yaml -f docker-compose.mac.yaml run test
  ```

* Without docker

  ```sh
  $ sbt gatling:test
  ```


## Reference

* [Share Compose configurations between files and projects](https://docs.docker.com/compose/extends/)

[1]: https://stackoverflow.com/questions/60430770/how-to-avoid-re-downloading-the-sbt-dependency-package-when-moving-the-sbt-proje#60456206
