# Realex HPP Java SDK
You can sign up for a Realex account at https://www.realexpayments.com.
## Requirements
Java 1.6 and later.
## Installation
### Maven users
Add this dependency to your project's POM:
```xml
<dependency>
  <groupId>com.realexpayments.hpp.sdk</groupId>
  <artifactId>rxp-hpp-java</artifactId>
  <version>1.0</version>
</dependency>
```

### Gradle users
Add this dependency to your project's build file:
```
compile "com.realexpayments.hpp.sdk:rxp-hpp-java:1.0"
```

## Usage
### Creating Request JSON for Realex JS SDK
```java
HppRequest hppRequest = new HppRequest()
						.addAmount(100)
 						.addCurrency("EUR")
 						.addMerchantId("merchantId");

RealexHpp realexHpp = new RealexHpp("mySecret");
String requestJson = realexHpp.requestToJson(hppRequest);
```
### Consuming Response JSON from Realex JS SDK
```java
RealexHpp realexHpp = new RealexHpp("mySecret");
HppResponse hppResponse = realexHpp.responseFromJson(responseJson); 
```
## License
See the LICENSE file.