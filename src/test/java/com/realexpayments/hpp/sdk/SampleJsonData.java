package com.realexpayments.hpp.sdk;

import java.util.HashMap;
import java.util.Map;

import org.junit.Assert;

import com.realexpayments.hpp.sdk.domain.HppRequest;
import com.realexpayments.hpp.sdk.domain.HppRequest.Flag;
import com.realexpayments.hpp.sdk.domain.HppResponse;

/**
 * Class containing sample JSON data and methods to check test data matches expected values.
 * 
 * @author markstanford
 *
 */
public class SampleJsonData {

	//sample JSON file paths
	public static final String VALID_HPP_REQUEST_JSON_PATH = "/sample-json/hpp-request-valid.json";
	public static final String VALID_HPP_RESPONSE_JSON_PATH = "/sample-json/hpp-response-valid.json";
	public static final String VALID_HPP_REQUEST_ENCODED_JSON_PATH = "/sample-json/hpp-request-encoded-valid.json";
	public static final String VALID_HPP_RESPONSE_ENCODED_JSON_PATH = "/sample-json/hpp-response-encoded-valid.json";
	public static final String UNKNOWN_DATA_HPP_REQUEST_JSON_PATH = "/sample-json/hpp-request-unknown-data.json";
	public static final String UNKNOWN_DATA_HPP_RESPONSE_JSON_PATH = "/sample-json/hpp-response-unknown-data.json";

	//valid JSON constants
	public static final String SECRET = "mysecret";
	public static final String ACCOUNT = "myAccount";
	public static final long AMOUNT = 100;
	public static final String COMMENT_ONE = "a-z A-Z 0-9 ' \", + “” ._ - & \\ / @ ! ? % ( )* : £ $ & € # [ ] | = ;ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷ø¤ùúûüýþÿŒŽšœžŸ¥";
	public static final String COMMENT_TWO = "Comment Two";

	//valid JSON constants HppRequest
	public static final String MERCHANT_ID = "MerchantID";
	public static final String TIMESTAMP = "20990101120000";
	public static final String AUTO_SETTLE_FLAG = Flag.TRUE.getFlag();
	public static final String BILLING_CODE = "123|56";
	public static final String BILLING_COUNTRY = "IRELAND";
	public static final String CARD_PAYMENT_BUTTON_TEXT = "Submit Payment";
	public static final String CARD_STORAGE_ENABLE = Flag.FALSE.getFlag();
	public static final String CURRENCY = "EUR";
	public static final String CUSTOMER_NUMBER = "123456";
	public static final String HASH_REQUEST = "5d8f05abd618e50db4861a61cc940112786474cf";
	public static final String LANGUAGE = "EN";
	public static final String OFFER_SAVE_CARD = Flag.FALSE.getFlag();
	public static final String ORDER_ID = "OrderID";
	public static final String PAYER_EXISTS = Flag.FALSE.getFlag();
	public static final String PAYER_REF = "PayerRef";
	public static final String PAYMENT_REF = "PaymentRef";
	public static final String PRODUCT_ID = "ProductID";
	public static final String RETURN_TSS = Flag.FALSE.getFlag();
	public static final String SHIPPING_CODE = "56|987";
	public static final String SHIPPING_COUNTRY = "IRELAND";
	public static final String VARIABLE_REFERENCE = "VariableRef";

	//valid JSON constants HppResponse
	public static final String ORDER_ID_RESPONSE = "ORD453-11";
	public static final String MERCHANT_ID_RESPONSE = "thestore";
	public static final String AUTH_CODE = "79347";
	public static final String BATCH_ID = "654321";
	public static final String CAVV = "123";
	public static final String CVN_RESULT = "1";
	public static final String ECI = "1";
	public static final String HASH_RESPONSE = "f093a0b233daa15f2bf44888f4fe75cb652e7bf0";
	public static final String MESSAGE = "Successful";
	public static final String PAS_REF = "3737468273643";
	public static final String RESULT = "00";
	public static final String XID = "654564564";
	public static final Map<String, String> TSS = generateTssResult();
	public static final String TSS_ONE_KEY = "TSS_1";
	public static final String TSS_ONE_VALUE = "TSS_1_VALUE";
	public static final String TSS_TWO_KEY = "TSS_2";
	public static final String TSS_TWO_VALUE = "TSS_2_VALUE";
	public static final String TIMESTAMP_RESPONSE = "20130814122239";

	//supplementary data (unknown values)
	public static final String UNKNOWN_ONE_KEY = "UNKNOWN_1";
	public static final String UNKNOWN_ONE_VALUE = "Unknown value 1";
	public static final String UNKNOWN_TWO_KEY = "UNKNOWN_2";
	public static final String UNKNOWN_TWO_VALUE = "Unknown value 2";
	public static final String UNKNOWN_THREE_KEY = "UNKNOWN_3";
	public static final String UNKNOWN_THREE_VALUE = "Unknown value 3";
	public static final String UNKNOWN_FOUR_KEY = "UNKNOWN_4";
	public static final String UNKNOWN_FOUR_VALUE = "Unknown value 4";
	public static final Map<String, String> SUPPLEMENTARY_DATA = generateSupplementaryData();

	/**
	 * Generates map of TSS data.
	 * 
	 * @return Map<String, String>
	 */
	public static Map<String, String> generateTssResult() {
		Map<String, String> tss = new HashMap<String, String>();

		tss.put(TSS_ONE_KEY, TSS_ONE_VALUE);
		tss.put(TSS_TWO_KEY, TSS_TWO_VALUE);

		return tss;
	}

	/**
	 * Generate map of supplementary data.
	 * 
	 * @return Map<String, String>
	 */
	public static Map<String, String> generateSupplementaryData() {
		Map<String, String> data = new HashMap<String, String>();

		data.put(UNKNOWN_ONE_KEY, UNKNOWN_ONE_VALUE);
		data.put(UNKNOWN_TWO_KEY, UNKNOWN_TWO_VALUE);
		data.put(UNKNOWN_THREE_KEY, UNKNOWN_THREE_VALUE);
		data.put(UNKNOWN_FOUR_KEY, UNKNOWN_FOUR_VALUE);

		return data;
	}

	/**
	 * Generates {@link HppRequest} object.
	 * 
	 * @return HppRequest
	 */
	public static HppRequest generateValidHppRequest() {
		HppRequest hppRequest = generateValidHppRequestWithEmptyDefaults()
				.addHash(HASH_REQUEST)
				.addOrderId(ORDER_ID)
				.addTimeStamp(TIMESTAMP);

		return hppRequest;
	}

	/**
	 * Generates {@link HppRequest} object with empty defaults (time stamp and order ID).
	 * 
	 * @return HppRequest
	 */
	public static HppRequest generateValidHppRequestWithEmptyDefaults() {
		HppRequest hppRequest = new HppRequest().addAccount(ACCOUNT)
				.addAmount(AMOUNT)
				.addAutoSettleFlag(AUTO_SETTLE_FLAG.equals(Flag.TRUE.getFlag()))
				.addBillingCode(BILLING_CODE)
				.addBillingCountry(BILLING_COUNTRY)
				.addCardPaymentButtonText(CARD_PAYMENT_BUTTON_TEXT)
				.addCardStorageEnable(CARD_STORAGE_ENABLE.equals(Flag.TRUE.getFlag()))
				.addCommentOne(COMMENT_ONE)
				.addCommentTwo(COMMENT_TWO)
				.addCurrency(CURRENCY)
				.addCustomerNumber(CUSTOMER_NUMBER)
				.addLanguage(LANGUAGE)
				.addMerchantId(MERCHANT_ID)
				.addOfferSaveCard(OFFER_SAVE_CARD.equals(Flag.TRUE.getFlag()))
				.addPayerExists(PAYER_EXISTS.equals(Flag.TRUE.getFlag()))
				.addPayerReference(PAYER_REF)
				.addPaymentReference(PAYMENT_REF)
				.addProductId(PRODUCT_ID)
				.addReturnTss(RETURN_TSS.equals(Flag.TRUE.getFlag()))
				.addShippingCode(SHIPPING_CODE)
				.addShippingCountry(SHIPPING_COUNTRY)
				.addVariableReference(VARIABLE_REFERENCE);

		hppRequest.setSupplementaryData(SUPPLEMENTARY_DATA);

		return hppRequest;
	}

	/**
	 * Generates valid {@link HppResponse} object.	 * 
	 * 
	 * @return HppResponse
	 */
	public static HppResponse generateValidHppResponse() {
		HppResponse hppResponse = new HppResponse();

		hppResponse.setAccount(ACCOUNT);
		hppResponse.setAmount(String.valueOf(AMOUNT));
		hppResponse.setAuthCode(AUTH_CODE);
		hppResponse.setBatchId(BATCH_ID);
		hppResponse.setCavv(CAVV);
		hppResponse.setCommentOne(COMMENT_ONE);
		hppResponse.setCommentTwo(COMMENT_TWO);
		hppResponse.setCvnResult(CVN_RESULT);
		hppResponse.setEci(ECI);
		hppResponse.setHash(HASH_RESPONSE);
		hppResponse.setMerchantId(MERCHANT_ID_RESPONSE);
		hppResponse.setMessage(MESSAGE);
		hppResponse.setOrderId(ORDER_ID_RESPONSE);
		hppResponse.setPasRef(PAS_REF);
		hppResponse.setResult(RESULT);
		hppResponse.setTimeStamp(TIMESTAMP_RESPONSE);
		hppResponse.setTss(TSS);
		hppResponse.setXid(XID);
		hppResponse.getSupplementaryData().putAll(SUPPLEMENTARY_DATA);

		return hppResponse;
	}

	/**
	 * Checks expected and converted {@link HppRequest} objects.
	 * 
	 * @param hppRequestExpected
	 * @param hppRequestConverted
	 * @param defaultsGenerated
	 */
	public static void checkValidHppRequest(HppRequest hppRequestExpected, HppRequest hppRequestConverted, boolean defaultsGenerated) {
		Assert.assertEquals("Json conversion incorrect Account", hppRequestExpected.getAccount(), hppRequestConverted.getAccount());
		Assert.assertEquals("Json conversion incorrect Amount", hppRequestExpected.getAmount(), hppRequestConverted.getAmount());
		Assert.assertEquals("Json conversion incorrect Auto Settle Flag", hppRequestExpected.getAutoSettleFlag(),
				hppRequestConverted.getAutoSettleFlag());
		Assert.assertEquals("Json conversion incorrect Billing Code", hppRequestExpected.getBillingCode(), hppRequestConverted.getBillingCode());
		Assert.assertEquals("Json conversion incorrect Billing Country", hppRequestExpected.getBillingCountry(),
				hppRequestConverted.getBillingCountry());
		Assert.assertEquals("Json conversion incorrect Card Payment Button Text", hppRequestExpected.getCardPaymentButtonText(),
				hppRequestConverted.getCardPaymentButtonText());
		Assert.assertEquals("Json conversion incorrect Card Storage Enable", hppRequestExpected.getCardStorageEnable(),
				hppRequestConverted.getCardStorageEnable());
		Assert.assertEquals("Json conversion incorrect Comment One", hppRequestExpected.getCommentOne(), hppRequestConverted.getCommentOne());
		Assert.assertEquals("Json conversion incorrect Comment Two", hppRequestExpected.getCommentTwo(), hppRequestConverted.getCommentTwo());
		Assert.assertEquals("Json conversion incorrect Currency", hppRequestExpected.getCurrency(), hppRequestConverted.getCurrency());
		Assert.assertEquals("Json conversion incorrect Customer Number", hppRequestExpected.getCustomerNumber(),
				hppRequestConverted.getCustomerNumber());
		Assert.assertEquals("Json conversion incorrect HPP Language", hppRequestExpected.getLanguage(), hppRequestConverted.getLanguage());
		Assert.assertEquals("Json conversion incorrect Merchant ID", hppRequestExpected.getMerchantId(), hppRequestConverted.getMerchantId());
		Assert.assertEquals("Json conversion incorrect Offer Save Card", hppRequestExpected.getOfferSaveCard(),
				hppRequestConverted.getOfferSaveCard());
		Assert.assertEquals("Json conversion incorrect Payer Exists", hppRequestExpected.getPayerExists(), hppRequestConverted.getPayerExists());
		Assert.assertEquals("Json conversion incorrect Payer Reference", hppRequestExpected.getPayerReference(),
				hppRequestConverted.getPayerReference());
		Assert.assertEquals("Json conversion incorrect Payment Reference", hppRequestExpected.getPaymentReference(),
				hppRequestConverted.getPaymentReference());
		Assert.assertEquals("Json conversion incorrect Product ID", hppRequestExpected.getProductId(), hppRequestConverted.getProductId());
		Assert.assertEquals("Json conversion incorrect Return TSS", hppRequestExpected.getReturnTss(), hppRequestConverted.getReturnTss());
		Assert.assertEquals("Json conversion incorrect Shipping Code", hppRequestExpected.getShippingCode(), hppRequestConverted.getShippingCode());
		Assert.assertEquals("Json conversion incorrect Shipping Country", hppRequestExpected.getShippingCountry(),
				hppRequestConverted.getShippingCountry());
		Assert.assertEquals("Json conversion incorrect Variable Reference", hppRequestExpected.getVariableReference(),
				hppRequestConverted.getVariableReference());

		if (!defaultsGenerated) {
			Assert.assertEquals("Json conversion incorrect Time Stamp", hppRequestExpected.getTimeStamp(), hppRequestConverted.getTimeStamp());
			Assert.assertEquals("Json conversion incorrect Hash", hppRequestExpected.getHash(), hppRequestConverted.getHash());
			Assert.assertEquals("Json conversion incorrect Order ID", hppRequestExpected.getOrderId(), hppRequestConverted.getOrderId());
		} else {
			Assert.assertNotNull("Time Stamp failed to generate", hppRequestConverted.getTimeStamp());
			Assert.assertNotNull("Hash failed to generate", hppRequestConverted.getHash());
			Assert.assertNotNull("Order ID failed to generate", hppRequestConverted.getOrderId());
		}

	}

	/**
	 * Checks expected and converted {@link HppResponse} objects.
	 * 
	 * @param hppResponseExpected
	 * @param hppResponseConverted
	 */
	public static void checkValidHppResponse(HppResponse hppResponseExpected, HppResponse hppResponseConverted) {
		Assert.assertEquals("Json conversion incorrect Account", hppResponseExpected.getAccount(), hppResponseConverted.getAccount());
		Assert.assertEquals("Json conversion incorrect Amount", hppResponseExpected.getAmount(), hppResponseConverted.getAmount());
		Assert.assertEquals("Json conversion incorrect Comment One", hppResponseExpected.getCommentOne(), hppResponseConverted.getCommentOne());
		Assert.assertEquals("Json conversion incorrect Comment Two", hppResponseExpected.getCommentTwo(), hppResponseConverted.getCommentTwo());
		Assert.assertEquals("Json conversion incorrect Merchant ID", hppResponseExpected.getMerchantId(), hppResponseConverted.getMerchantId());
		Assert.assertEquals("Json conversion incorrect Time Stamp", hppResponseExpected.getTimeStamp(), hppResponseConverted.getTimeStamp());
		Assert.assertEquals("Json conversion incorrect Hash", hppResponseExpected.getHash(), hppResponseConverted.getHash());
		Assert.assertEquals("Json conversion incorrect Order ID", hppResponseExpected.getOrderId(), hppResponseConverted.getOrderId());
		Assert.assertEquals("Json conversion incorrect Auth Code", hppResponseExpected.getAuthCode(), hppResponseConverted.getAuthCode());
		Assert.assertEquals("Json conversion incorrect Batch ID", hppResponseExpected.getBatchId(), hppResponseConverted.getBatchId());
		Assert.assertEquals("Json conversion incorrect CAVV", hppResponseExpected.getCavv(), hppResponseConverted.getCavv());
		Assert.assertEquals("Json conversion incorrect CVN Result", hppResponseExpected.getCvnResult(), hppResponseConverted.getCvnResult());
		Assert.assertEquals("Json conversion incorrect ECI", hppResponseExpected.getEci(), hppResponseConverted.getEci());
		Assert.assertEquals("Json conversion incorrect Message", hppResponseExpected.getMessage(), hppResponseConverted.getMessage());
		Assert.assertEquals("Json conversion incorrect Pas Ref", hppResponseExpected.getPasRef(), hppResponseConverted.getPasRef());
		Assert.assertEquals("Json conversion incorrect Result", hppResponseExpected.getResult(), hppResponseConverted.getResult());
		Assert.assertEquals("Json conversion incorrect XID", hppResponseExpected.getXid(), hppResponseConverted.getXid());
		Assert.assertEquals("Json conversion incorrect TSS Entry", hppResponseConverted.getTss().get(TSS_ONE_KEY),
				hppResponseConverted.getTss().get(TSS_ONE_KEY));
		Assert.assertEquals("Json conversion incorrect TSS Entry", hppResponseConverted.getTss().get(TSS_TWO_KEY),
				hppResponseConverted.getTss().get(TSS_TWO_KEY));
	}

	/**
	 * Checks response supplementary data matches expected values.
	 * 
	 * @param hppResponseConverted
	 */
	public static void checkValidHppResponseSupplementaryData(HppResponse hppResponseConverted) {
		Assert.assertEquals("Json conversion incorrect Unknown one", UNKNOWN_ONE_VALUE,
				hppResponseConverted.getSupplementaryData().get(UNKNOWN_ONE_KEY));
		Assert.assertEquals("Json conversion incorrect Unknown one", UNKNOWN_TWO_VALUE,
				hppResponseConverted.getSupplementaryData().get(UNKNOWN_TWO_KEY));
		Assert.assertEquals("Json conversion incorrect Unknown one", UNKNOWN_THREE_VALUE,
				hppResponseConverted.getSupplementaryData().get(UNKNOWN_THREE_KEY));
		Assert.assertEquals("Json conversion incorrect Unknown one", UNKNOWN_FOUR_VALUE,
				hppResponseConverted.getSupplementaryData().get(UNKNOWN_FOUR_KEY));
	}

	/**
	 * Checks request supplementary data matches expected values. 
	 * 
	 * @param hppRequestConverted
	 */
	public static void checkValidHppRequestSupplementaryData(HppRequest hppRequestConverted) {
		Assert.assertEquals("Json conversion incorrect Unknown one", UNKNOWN_ONE_VALUE,
				hppRequestConverted.getSupplementaryData().get(UNKNOWN_ONE_KEY));
		Assert.assertEquals("Json conversion incorrect Unknown one", UNKNOWN_TWO_VALUE,
				hppRequestConverted.getSupplementaryData().get(UNKNOWN_TWO_KEY));
		Assert.assertEquals("Json conversion incorrect Unknown one", UNKNOWN_THREE_VALUE,
				hppRequestConverted.getSupplementaryData().get(UNKNOWN_THREE_KEY));
		Assert.assertEquals("Json conversion incorrect Unknown one", UNKNOWN_FOUR_VALUE,
				hppRequestConverted.getSupplementaryData().get(UNKNOWN_FOUR_KEY));
	}

}
