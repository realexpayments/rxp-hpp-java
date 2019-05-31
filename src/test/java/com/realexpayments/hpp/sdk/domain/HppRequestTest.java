package com.realexpayments.hpp.sdk.domain;

import org.junit.Assert;
import org.junit.Test;

import com.realexpayments.hpp.sdk.RealexHpp;
import com.realexpayments.hpp.sdk.SampleJsonData;

/**
 * Tests for HppRequest methods.
 * 
 * @author markstanford
 *
 */
public class HppRequestTest {

	private static final String TIMESTAMP = "20130814122239";
	private static final String MERCHANT_ID = "thestore";
	private static final String ORDER_ID = "ORD453-11";
	private static final String AMOUNT = "29900";
	private static final String CURRENCY = "EUR";
	private static final String PAYER_REFERENCE = "newpayer1";
	private static final String PAYMENT_REFERENCE = "mycard1";
	private static final String FRAUD_FILTER_MODE_ACTIVE = "ACTIVE";
	private static final String HPP_SELECT_STORED_CARD = "2b8de093-0241-4985-ad96-76ca0b26b478";

	/**
	 * Test generating security hash.
	 */
	@Test
	public void hashTest() {

		HppRequest hppRequest = SampleJsonData.generateValidHppRequest(false);
		hppRequest.setTimeStamp(TIMESTAMP);
		hppRequest.setMerchantId(MERCHANT_ID);
		hppRequest.setOrderId(ORDER_ID);
		hppRequest.setAmount(AMOUNT);
		hppRequest.setCurrency(CURRENCY);

		String expectedHash = "cc72c08e529b3bc153481eda9533b815cef29de3";
		String actualHash = hppRequest.hash("mysecret").getHash();

		Assert.assertEquals("Card storage hash does not match expected.", expectedHash, actualHash);

	}

	/**
	 * Test generating security hash when select stored card present.
	 */
	@Test
	public void hppSelectStoredCardShouldOverridePayerRef() {

		HppRequest hppRequest = SampleJsonData.generateValidHppRequest(false);
		hppRequest.setPayerReference(PAYER_REFERENCE);

		HppRequest hppRequest2 = SampleJsonData.generateValidHppRequest(false);
		hppRequest2.setPayerReference(PAYER_REFERENCE);
		hppRequest2.addHppSelectStoredCard(HPP_SELECT_STORED_CARD);

		String hash1 = hppRequest.hash("mysecret").getHash();
		String hash2 = hppRequest2.hash("mysecret").getHash();

		Assert.assertNotEquals(hash1, hash2);

	}

	/**
	 * Test generating security hash when select stored card present but an empty string.
	 */
	@Test
	public void hppSelectStoredCardEmptyStringShouldNotOverridePayerRef() {

		HppRequest hppRequest = SampleJsonData.generateValidHppRequest(false);
		hppRequest.setPayerReference(PAYER_REFERENCE);

		HppRequest hppRequest2 = SampleJsonData.generateValidHppRequest(false);
		hppRequest2.setPayerReference(PAYER_REFERENCE);
		hppRequest2.addHppSelectStoredCard("");

		String hash1 = hppRequest.hash("mysecret").getHash();
		String hash2 = hppRequest2.hash("mysecret").getHash();

		Assert.assertEquals(hash1, hash2);

	}

	/**
	 * Test generating security hash for card store.
	 */
	@Test
	public void cardStoreHashTest() {

		HppRequest hppRequest = SampleJsonData.generateValidHppRequest(true);
		hppRequest.setTimeStamp(TIMESTAMP);
		hppRequest.setMerchantId(MERCHANT_ID);
		hppRequest.setOrderId(ORDER_ID);
		hppRequest.setAmount(AMOUNT);
		hppRequest.setCurrency(CURRENCY);
		hppRequest.setPayerReference(PAYER_REFERENCE);
		hppRequest.setPaymentReference(PAYMENT_REFERENCE);

		String expectedHash = "4106afc4666c6145b623089b1ad4098846badba2";
		String actualHash = hppRequest.hash("mysecret").getHash();

		Assert.assertEquals("Card storage hash does not match expected.", expectedHash, actualHash);

	}

	/**
	 * Test generating security hash with HPP fraud filter mode.
	 */
	@Test
	public void hashTestWithHppFraudFilterMode() {

		HppRequest hppRequest = SampleJsonData.generateValidHppRequest(false);
		hppRequest.setTimeStamp(TIMESTAMP);
		hppRequest.setMerchantId(MERCHANT_ID);
		hppRequest.setOrderId(ORDER_ID);
		hppRequest.setAmount(AMOUNT);
		hppRequest.setCurrency(CURRENCY);
		hppRequest.setHppFraudFilterMode(FRAUD_FILTER_MODE_ACTIVE);

		String expectedHash = "b7b3cbb60129a1c169a066afa09ce7cc843ff1c1";
		String actualHash = hppRequest.hash("mysecret").getHash();

		Assert.assertEquals("Card storage hash does not match expected.", expectedHash, actualHash);

	}

	/**
	 * Test generating security hash for card store with HPP fraud filter mode.
	 */
	@Test
	public void cardStoreHashTestWithHppFraudFilterMode() {

		HppRequest hppRequest = SampleJsonData.generateValidHppRequest(true);
		hppRequest.setTimeStamp(TIMESTAMP);
		hppRequest.setMerchantId(MERCHANT_ID);
		hppRequest.setOrderId(ORDER_ID);
		hppRequest.setAmount(AMOUNT);
		hppRequest.setCurrency(CURRENCY);
		hppRequest.setPayerReference(PAYER_REFERENCE);
		hppRequest.setPaymentReference(PAYMENT_REFERENCE);
		hppRequest.setHppFraudFilterMode(FRAUD_FILTER_MODE_ACTIVE);

		String expectedHash = "39f637a321da4ebc3a433ed327b2c2921ad58fdb";
		String actualHash = hppRequest.hash("mysecret").getHash();

		Assert.assertEquals("Card storage hash does not match expected.", expectedHash, actualHash);

	}
	
	/**
	 * Test new 3D Secure 2 fields
	 * @return 
	 */
	@Test
	public void newThreeDSecureTwoFields() {

		HppRequest hppRequest = new HppRequest()
				.addTimeStamp("20190531113558")
				.addMerchantId("MerchantId")
				.addAccount("internet")
				.addOrderId("4lOCnXGhSiCMuq-P4PGqiQ")
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
		
		String requestJson = realexHpp.requestToJson(hppRequest, false);
		
		String expectedJson = "{\"MERCHANT_ID\":\"MerchantId\",\"ACCOUNT\":\"internet\",\"ORDER_ID\":\"4lOCnXGhSiCMuq-P4PGqiQ\",\"AMOUNT\":\"1001\",\"CURRENCY\":\"EUR\",\"TIMESTAMP\":\"20190531113558\",\"SHA1HASH\":\"57f3ec83ad2634816bf00a5688c0b82b9d608ff2\",\"AUTO_SETTLE_FLAG\":\"1\",\"HPP_CUSTOMER_EMAIL\":\"james.mason@example.com\",\"HPP_CUSTOMER_PHONENUMBER_MOBILE\":\"44|07123456789\",\"HPP_BILLING_STREET1\":\"Flat 123\",\"HPP_BILLING_STREET2\":\"House 456\",\"HPP_BILLING_STREET3\":\"Unit 4\",\"HPP_BILLING_CITY\":\"Halifax\",\"HPP_BILLING_POSTALCODE\":\"W5 9HR\",\"HPP_BILLING_COUNTRY\":\"826\",\"HPP_SHIPPING_STREET1\":\"Apartment 825\",\"HPP_SHIPPING_STREET2\":\"Complex 741\",\"HPP_SHIPPING_STREET3\":\"House 963\",\"HPP_SHIPPING_CITY\":\"Chicago\",\"HPP_SHIPPING_STATE\":\"IL\",\"HPP_SHIPPING_POSTALCODE\":\"50001\",\"HPP_SHIPPING_COUNTRY\":\"840\"}";

		Assert.assertEquals("Incorrect request JSON.", requestJson, expectedJson);
	}
}
