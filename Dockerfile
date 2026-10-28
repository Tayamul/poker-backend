# Stage 1: Build the application
FROM sbtscala/scala-sbt:eclipse-temurin-21_1.x AS build

WORKDIR /app
COPY . .

RUN sbt assembly

# Stage 2: Run the application
FROM eclipse-temurin:21-jre

WORKDIR /app

COPY --from=build /app/target/scala-3.3.1/poker-assembly-0.1.0-SNAPSHOT.jar app.jar

# Limit the Java heap to help fit within a small instance
ENV JAVA_TOOL_OPTIONS="-XX:MaxRAMPercentage=60.0"

EXPOSE 10000

CMD ["java", "-jar", "app.jar"]
