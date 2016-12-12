# Realex HPP Java SDK
You can sign up for a Realex Payments account at https://developer.realexpayments.com.
## Requirements
Java 1.6 and later.
## Installation
### Maven users
Add this dependency to your project's POM:
```xml
<dependency>
  <groupId>com.realexpayments.hpp.sdk</groupId>
  <artifactId>rxp-hpp-java</artifactId>
  <version>1.3</version>
</dependency>
```

### Gradle users
Add this dependency to your project's build file:
```
compile "com.realexpayments.hpp.sdk:rxp-hpp-java:1.3"
```

## Usage
### Creating Request JSON for Realex JS Library
```java
HppRequest request = new HppRequest()
						.addAmount(100)
 						.addCurrency("EUR")
 						.addMerchantId("merchantId");

RealexHpp realexHpp = new RealexHpp("mySecret");
String requestJson = realexHpp.requestToJson(request);
```
### Consuming Response JSON from Realex JS SDK
```java
RealexHpp realexHpp = new RealexHpp("mySecret");
HppResponse hppResponse = realexHpp.responseFromJson(responseJson); 
```
## License
See the LICENSE file.