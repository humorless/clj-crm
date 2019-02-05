FROM openjdk:8-alpine

COPY target/uberjar/clj-crm.jar /clj-crm/app.jar

EXPOSE 3000

CMD ["java", "-jar", "/clj-crm/app.jar"]
