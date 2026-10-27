FROM sbtscala/scala-sbt:eclipse-temurin-21_1.x

WORKDIR /app

COPY . .

RUN sbt compile

CMD ["sbt", "run"]
