# Please use our new Java SDK
We've moved. We highly recommend you use the Global Payments Java SDK
which supports all the features of this SDK and will benefit from all future releases:
https://github.com/globalpayments/java-sdk

With the latest update (1.3.4) this SDK supports the mandatory and recommend HPP fields for 3D Secure 2. Going forward it will only receive critical security updates, no further feature updates will be released beyond 3D Secure 2.
# Realex Payments HPP Java SDK
You can sign up for a Global Payments (formerly Realex Payments) account at https://developer.globalpay.com
## Requirements
Java 1.6 and later.
## Installation
### Maven users
Add this dependency to your project's POM:
```xml
<dependency>
  <groupId>com.realexpayments.hpp.sdk</groupId>
  <artifactId>rxp-hpp-java</artifactId>
  <version>1.3.4</version>
</dependency>
```

### Gradle users
Add this dependency to your project's build file:
```
compile "com.realexpayments.hpp.sdk:rxp-hpp-java:1.3.4"
```

## Usage
### Create HPP Request JSON for Realex Payments JS Library
```java
HppRequest hppRequest = new HppRequest()
		.addMerchantId("MerchantId")
		.addAccount("internet")
		.addAmount(1001)
		.addCurrency("EUR")
		.addAutoSettleFlag(true)
		// 3D Secure 2 Mandatory and Recommended Fields
		.addCustomerEmail("james.mason@example.com")
		.addCustomerPhoneMobile("44|07123456789")
		.addBillingAddress1("Flat 123")
		.addBillingAddress2("House 456")
		.addBillingAddress3("Unit 4")
		.addBillingAddressCity("Halifax")
		.addBillingAddressPostalCode("W5 9HR")
		.addBillingAddressCountry("826")
		.addShippingAddress1("Apartment 825")
		.addShippingAddress2("Complex 741")
		.addShippingAddress3("House 963")
		.addShippingAddressCity("Chicago")
		.addShippingAddressState("IL")
		.addShippingAddressPostalCode("50001")
		.addShippingAddressCountry("840");

RealexHpp realexHpp = new RealexHpp("secret");

try {
	String requestJson = realexHpp.requestToJson(hppRequest, false);
	// TODO: pass the HPP request JSON to the JavaScript, iOS or Android Library
		
}
catch (RealexValidationException e) {
	// TODO: Add your error handling here
}
catch (RealexException e) {
	// TODO: Add your error handling here
}
```
### Consuming HPP Response JSON from Realex Payments JS Library
```java
RealexHpp realexHpp = new RealexHpp("secret");
HppResponse hppResponse = realexHpp.responseFromJson(responseJson); 
```
## License
See the LICENSE file.
