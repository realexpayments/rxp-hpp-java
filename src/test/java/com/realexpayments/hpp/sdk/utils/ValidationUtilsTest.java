package com.realexpayments.hpp.sdk.utils;

import java.util.Arrays;
import java.util.ResourceBundle;

import org.junit.Assert;
import org.junit.BeforeClass;
import org.junit.Test;

import com.realexpayments.hpp.sdk.RealexValidationException;
import com.realexpayments.hpp.sdk.SampleJsonData;
import com.realexpayments.hpp.sdk.domain.HppRequest;
import com.realexpayments.hpp.sdk.domain.HppRequest.Flag;

/**
 * Test class for validating {@link HppRequest}.
 * 
 * @author markstanford
 */
public class ValidationUtilsTest {

	/**
	 * Validation message bundle.
	 */
	private static ResourceBundle VALIDATION_MESSAGES;

	/**
	 * Instantiate validation messages bundle.
	 */
	@BeforeClass
	public static void runOnce() {
		VALIDATION_MESSAGES = ResourceBundle.getBundle("ValidationMessages");
	}

	/**
	 * Test validation passed.
	 */
	@Test
	public void validationPassedTest() {
		HppRequest hppRequest = SampleJsonData.generateValidHppRequest();
		hppRequest.generateDefaults(SampleJsonData.SECRET);

		try {
			ValidationUtils.validate(hppRequest);
		} catch (RealexValidationException ex) {
			Assert.fail("This HppRequest should have no validation errors.");
		}
	}

	/**
	 * Test merchant ID.
	 */
	@Test
	public void merchantIdTest() {
		HppRequest hppRequest = SampleJsonData.generateValidHppRequest();
		hppRequest.generateDefaults(SampleJsonData.SECRET);

		hppRequest.setMerchantId("");

		try {
			ValidationUtils.validate(hppRequest);
			Assert.fail("This HppRequest should have validation errors.");
		} catch (RealexValidationException ex) {
			Assert.assertEquals(VALIDATION_MESSAGES.getString("hppRequest.merchantId.size"), ex.getValidationMessages().get(0));
		}

		char[] charsAtMax = new char[50];
		Arrays.fill(charsAtMax, '1');
		hppRequest.setMerchantId(new String(charsAtMax));

		try {
			ValidationUtils.validate(hppRequest);
		} catch (RealexValidationException ex) {
			Assert.fail("This HppRequest should not have validation errors.");
		}

		char[] charsOverMax = new char[51];
		Arrays.fill(charsOverMax, '1');
		hppRequest.setMerchantId(new String(charsOverMax));

		try {
			ValidationUtils.validate(hppRequest);
			Assert.fail("This HppRequest should have validation errors.");
		} catch (RealexValidationException ex) {
			Assert.assertEquals(VALIDATION_MESSAGES.getString("hppRequest.merchantId.size"), ex.getValidationMessages().get(0));
		}

		hppRequest.setMerchantId("azAZ09.");

		try {
			ValidationUtils.validate(hppRequest);
		} catch (RealexValidationException ex) {
			Assert.fail("This HppRequest should have no validation errors.");
		}

		hppRequest.setMerchantId("$&^*");

		try {
			ValidationUtils.validate(hppRequest);
			Assert.fail("This HppRequest should have validation errors.");
		} catch (RealexValidationException ex) {
			Assert.assertEquals(VALIDATION_MESSAGES.getString("hppRequest.merchantId.pattern"), ex.getValidationMessages().get(0));
		}
	}

	/**
	 * Test account.
	 */
	@Test
	public void accountTest() {
		HppRequest hppRequest = SampleJsonData.generateValidHppRequest();
		hppRequest.generateDefaults(SampleJsonData.SECRET);

		hppRequest.setAccount("");

		try {
			ValidationUtils.validate(hppRequest);
		} catch (RealexValidationException ex) {
			Assert.fail("This HppRequest should have no validation errors.");
		}

		hppRequest.setAccount("azAZ09");

		try {
			ValidationUtils.validate(hppRequest);
		} catch (RealexValidationException ex) {
			Assert.fail("This HppRequest should have no validation errors.");
		}

		char[] charsAtMax = new char[30];
		Arrays.fill(charsAtMax, '1');
		hppRequest.setAccount(new String(charsAtMax));

		try {
			ValidationUtils.validate(hppRequest);
		} catch (RealexValidationException ex) {
			Assert.fail("This HppRequest should not have validation errors.");
		}

		char[] charsOverMax = new char[31];
		Arrays.fill(charsOverMax, '1');
		hppRequest.setAccount(new String(charsOverMax));

		try {
			ValidationUtils.validate(hppRequest);
			Assert.fail("This HppRequest should have validation errors.");
		} catch (RealexValidationException ex) {
			Assert.assertEquals(VALIDATION_MESSAGES.getString("hppRequest.account.size"), ex.getValidationMessages().get(0));
		}

		hppRequest.setAccount("$&^*");

		try {
			ValidationUtils.validate(hppRequest);
			Assert.fail("This HppRequest should have validation errors.");
		} catch (RealexValidationException ex) {
			Assert.assertEquals(VALIDATION_MESSAGES.getString("hppRequest.account.pattern"), ex.getValidationMessages().get(0));
		}
	}

	/**
	 * Test order ID.
	 */
	@Test
	public void orderIdTest() {
		HppRequest hppRequest = SampleJsonData.generateValidHppRequest();
		hppRequest.generateDefaults(SampleJsonData.SECRET);

		hppRequest.setOrderId("");

		try {
			ValidationUtils.validate(hppRequest);
		} catch (RealexValidationException ex) {
			Assert.fail("This HppRequest should have validation errors.");
		}

		hppRequest.setOrderId("azAZ09_-");

		try {
			ValidationUtils.validate(hppRequest);
		} catch (RealexValidationException ex) {
			Assert.fail("This HppRequest should have no validation errors.");
		}

		char[] charsAtMax = new char[50];
		Arrays.fill(charsAtMax, '1');
		hppRequest.setOrderId(new String(charsAtMax));

		try {
			ValidationUtils.validate(hppRequest);
		} catch (RealexValidationException ex) {
			Assert.fail("This HppRequest should not have validation errors.");
		}

		char[] charsOverMax = new char[51];
		Arrays.fill(charsOverMax, '1');
		hppRequest.setOrderId(new String(charsOverMax));

		try {
			ValidationUtils.validate(hppRequest);
			Assert.fail("This HppRequest should have validation errors.");
		} catch (RealexValidationException ex) {
			Assert.assertEquals(VALIDATION_MESSAGES.getString("hppRequest.orderId.size"), ex.getValidationMessages().get(0));
		}

		hppRequest.setOrderId("$&^*");

		try {
			ValidationUtils.validate(hppRequest);
			Assert.fail("This HppRequest should have validation errors.");
		} catch (RealexValidationException ex) {
			Assert.assertEquals(VALIDATION_MESSAGES.getString("hppRequest.orderId.pattern"), ex.getValidationMessages().get(0));
		}
	}

	/**
	 * Test amount.
	 */
	@Test
	public void amountTest() {
		HppRequest hppRequest = SampleJsonData.generateValidHppRequest();
		hppRequest.generateDefaults(SampleJsonData.SECRET);

		hppRequest.setAmount("");

		try {
			ValidationUtils.validate(hppRequest);
			Assert.fail("This HppRequest should have validation errors.");
		} catch (RealexValidationException ex) {
			Assert.assertEquals(VALIDATION_MESSAGES.getString("hppRequest.amount.size"), ex.getValidationMessages().get(0));
		}

		char[] charsAtMax = new char[11];
		Arrays.fill(charsAtMax, '1');
		hppRequest.setAmount(new String(charsAtMax));

		try {
			ValidationUtils.validate(hppRequest);
		} catch (RealexValidationException ex) {
			Assert.fail("This HppRequest should not have validation errors.");
		}

		char[] charsOverMax = new char[12];
		Arrays.fill(charsOverMax, '1');
		hppRequest.setAmount(new String(charsOverMax));

		try {
			ValidationUtils.validate(hppRequest);
			Assert.fail("This HppRequest should have validation errors.");
		} catch (RealexValidationException ex) {
			Assert.assertEquals(VALIDATION_MESSAGES.getString("hppRequest.amount.size"), ex.getValidationMessages().get(0));
		}

		hppRequest.setAmount("abc");

		try {
			ValidationUtils.validate(hppRequest);
			Assert.fail("This HppRequest should have validation errors.");
		} catch (RealexValidationException ex) {
			Assert.assertEquals(VALIDATION_MESSAGES.getString("hppRequest.amount.pattern"), ex.getValidationMessages().get(0));
		}

		hppRequest.setValidateCardOnly(Flag.TRUE.getFlag());
		hppRequest.setAmount("0");

		try {
			ValidationUtils.validate(hppRequest);
		} catch (RealexValidationException ex) {
			Assert.fail("This HppRequest should have validation errors.");
		}

		hppRequest.setValidateCardOnly(Flag.TRUE.getFlag());
		hppRequest.setAmount("1");

		try {
			ValidationUtils.validate(hppRequest);
			Assert.fail("This HppRequest should have validation errors.");
		} catch (RealexValidationException ex) {
			Assert.assertEquals(VALIDATION_MESSAGES.getString("hppRequest.amount.otb"), ex.getValidationMessages().get(0));
		}
	}

	/**
	 * Test currency.
	 */
	@Test
	public void currencyTest() {
		HppRequest hppRequest = SampleJsonData.generateValidHppRequest();
		hppRequest.generateDefaults(SampleJsonData.SECRET);

		hppRequest.setCurrency("");

		try {
			ValidationUtils.validate(hppRequest);
			Assert.fail("This HppRequest should have validation errors.");
		} catch (RealexValidationException ex) {
			Assert.assertEquals(VALIDATION_MESSAGES.getString("hppRequest.currency.size"), ex.getValidationMessages().get(0));
		}

		hppRequest.setCurrency("EuR");

		try {
			ValidationUtils.validate(hppRequest);
		} catch (RealexValidationException ex) {
			Assert.fail("This HppRequest should have no validation errors.");
		}

		hppRequest.setCurrency("abcd");

		try {
			ValidationUtils.validate(hppRequest);
			Assert.fail("This HppRequest should have validation errors.");
		} catch (RealexValidationException ex) {
			Assert.assertEquals(VALIDATION_MESSAGES.getString("hppRequest.currency.size"), ex.getValidationMessages().get(0));
		}

		hppRequest.setCurrency("ab1");

		try {
			ValidationUtils.validate(hppRequest);
			Assert.fail("This HppRequest should have validation errors.");
		} catch (RealexValidationException ex) {
			Assert.assertEquals(VALIDATION_MESSAGES.getString("hppRequest.currency.pattern"), ex.getValidationMessages().get(0));
		}
	}

	/**
	 * Test time stamp.
	 */
	@Test
	public void timeStampTest() {
		HppRequest hppRequest = SampleJsonData.generateValidHppRequest();
		hppRequest.generateDefaults(SampleJsonData.SECRET);

		hppRequest.setTimeStamp("");

		try {
			ValidationUtils.validate(hppRequest);
			Assert.fail("This HppRequest should have validation errors.");
		} catch (RealexValidationException ex) {
			Assert.assertEquals(VALIDATION_MESSAGES.getString("hppRequest.timestamp.size"), ex.getValidationMessages().get(0));
		}

		char[] charsAtMax = new char[14];
		Arrays.fill(charsAtMax, '1');
		hppRequest.setTimeStamp(new String(charsAtMax));

		try {
			ValidationUtils.validate(hppRequest);
		} catch (RealexValidationException ex) {
			Assert.fail("This HppRequest should not have validation errors.");
		}

		char[] charsOverMax = new char[15];
		Arrays.fill(charsOverMax, '1');
		hppRequest.setTimeStamp(new String(charsOverMax));

		try {
			ValidationUtils.validate(hppRequest);
			Assert.fail("This HppRequest should have validation errors.");
		} catch (RealexValidationException ex) {
			Assert.assertEquals(VALIDATION_MESSAGES.getString("hppRequest.timestamp.size"), ex.getValidationMessages().get(0));
		}

		hppRequest.setTimeStamp("1234567890123a");

		try {
			ValidationUtils.validate(hppRequest);
			Assert.fail("This HppRequest should have validation errors.");
		} catch (RealexValidationException ex) {
			Assert.assertEquals(VALIDATION_MESSAGES.getString("hppRequest.timestamp.pattern"), ex.getValidationMessages().get(0));
		}
	}

	/**
	 * Test hash.
	 */
	@Test
	public void hashTest() {
		HppRequest hppRequest = SampleJsonData.generateValidHppRequest();
		hppRequest.generateDefaults(SampleJsonData.SECRET);

		hppRequest.setHash("");

		try {
			ValidationUtils.validate(hppRequest);
			Assert.fail("This HppRequest should have validation errors.");
		} catch (RealexValidationException ex) {
			Assert.assertEquals(VALIDATION_MESSAGES.getString("hppRequest.hash.size"), ex.getValidationMessages().get(0));
		}

		char[] charsAtMax = new char[40];
		Arrays.fill(charsAtMax, 'a');
		hppRequest.setHash(new String(charsAtMax));

		try {
			ValidationUtils.validate(hppRequest);
		} catch (RealexValidationException ex) {
			Assert.fail("This HppRequest should not have validation errors.");
		}

		char[] charsOverMax = new char[41];
		Arrays.fill(charsOverMax, 'a');
		hppRequest.setHash(new String(charsOverMax));

		try {
			ValidationUtils.validate(hppRequest);
			Assert.fail("This HppRequest should have validation errors.");
		} catch (RealexValidationException ex) {
			Assert.assertEquals(VALIDATION_MESSAGES.getString("hppRequest.hash.size"), ex.getValidationMessages().get(0));
		}

		hppRequest.setHash("5d8f05abd618e50db4861a61cc940112786474c_");

		try {
			ValidationUtils.validate(hppRequest);
			Assert.fail("This HppRequest should have validation errors.");
		} catch (RealexValidationException ex) {
			Assert.assertEquals(VALIDATION_MESSAGES.getString("hppRequest.hash.pattern"), ex.getValidationMessages().get(0));
		}
	}

	/**
	 * Test auto settle flag.
	 */
	@Test
	public void autoSettleFlagTest() {
		HppRequest hppRequest = SampleJsonData.generateValidHppRequest();
		hppRequest.generateDefaults(SampleJsonData.SECRET);

		hppRequest.setAutoSettleFlag(null);

		try {
			ValidationUtils.validate(hppRequest);
		} catch (RealexValidationException ex) {
			Assert.fail("This HppRequest should have no validation errors.");
		}

		hppRequest.setAutoSettleFlag("");

		try {
			ValidationUtils.validate(hppRequest);
		} catch (RealexValidationException ex) {
			Assert.fail("This HppRequest should have no validation errors.");
		}

		hppRequest.setAutoSettleFlag("0");

		try {
			ValidationUtils.validate(hppRequest);
		} catch (RealexValidationException ex) {
			Assert.fail("This HppRequest should have no validation errors.");
		}

		hppRequest.setAutoSettleFlag("1");

		try {
			ValidationUtils.validate(hppRequest);
		} catch (RealexValidationException ex) {
			Assert.fail("This HppRequest should have no validation errors.");
		}

		hppRequest.setAutoSettleFlag("on");

		try {
			ValidationUtils.validate(hppRequest);
		} catch (RealexValidationException ex) {
			Assert.fail("This HppRequest should have no validation errors.");
		}

		hppRequest.setAutoSettleFlag("off");

		try {
			ValidationUtils.validate(hppRequest);
		} catch (RealexValidationException ex) {
			Assert.fail("This HppRequest should have no validation errors.");
		}

		hppRequest.setAutoSettleFlag("multi");

		try {
			ValidationUtils.validate(hppRequest);
		} catch (RealexValidationException ex) {
			Assert.fail("This HppRequest should have no validation errors.");
		}

		hppRequest.setAutoSettleFlag("ON");

		try {
			ValidationUtils.validate(hppRequest);
		} catch (RealexValidationException ex) {
			Assert.fail("This HppRequest should have no validation errors.");
		}

		hppRequest.setAutoSettleFlag("OFF");

		try {
			ValidationUtils.validate(hppRequest);
		} catch (RealexValidationException ex) {
			Assert.fail("This HppRequest should have no validation errors.");
		}

		hppRequest.setAutoSettleFlag("MULTI");

		try {
			ValidationUtils.validate(hppRequest);
		} catch (RealexValidationException ex) {
			Assert.fail("This HppRequest should have no validation errors.");
		}

		hppRequest.setAutoSettleFlag("MuLtI");

		try {
			ValidationUtils.validate(hppRequest);
		} catch (RealexValidationException ex) {
			Assert.fail("This HppRequest should have no validation errors.");
		}

		hppRequest.setAutoSettleFlag("a");

		try {
			ValidationUtils.validate(hppRequest);
			Assert.fail("This HppRequest should have validation errors.");
		} catch (RealexValidationException ex) {
			Assert.assertEquals(VALIDATION_MESSAGES.getString("hppRequest.autoSettleFlag.pattern"), ex.getValidationMessages().get(0));
		}
	}

	/**
	 * Test comment one.
	 */
	@Test
	public void commentOneTest() {
		HppRequest hppRequest = SampleJsonData.generateValidHppRequest();
		hppRequest.generateDefaults(SampleJsonData.SECRET);

		hppRequest.setCommentOne("");

		try {
			ValidationUtils.validate(hppRequest);
		} catch (RealexValidationException ex) {
			Assert.fail("This HppRequest should have no validation errors.");
		}

		hppRequest
				.setCommentOne("a-z A-Z 0-9 ' \", + “” ._ - & \\ / @ ! ? % ( )* : £ $ & € # [ ] | = ;ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷ø¤ùúûüýþÿŒŽšœžŸ¥");

		try {
			ValidationUtils.validate(hppRequest);
		} catch (RealexValidationException ex) {
			Assert.fail("This HppRequest should have no validation errors.");
		}

		char[] charsAtMax = new char[255];
		Arrays.fill(charsAtMax, '1');
		hppRequest.setCommentOne(new String(charsAtMax));

		try {
			ValidationUtils.validate(hppRequest);
		} catch (RealexValidationException ex) {
			Assert.fail("This HppRequest should not have validation errors.");
		}

		char[] charsOverMax = new char[256];
		Arrays.fill(charsOverMax, 'a');
		hppRequest.setCommentOne(new String(charsOverMax));

		try {
			ValidationUtils.validate(hppRequest);
			Assert.fail("This HppRequest should have validation errors.");
		} catch (RealexValidationException ex) {
			Assert.assertEquals(VALIDATION_MESSAGES.getString("hppRequest.comment1.size"), ex.getValidationMessages().get(0));
		}
	}

	/**
	 * Test comment two.
	 */
	@Test
	public void commentTwoTest() {
		HppRequest hppRequest = SampleJsonData.generateValidHppRequest();
		hppRequest.generateDefaults(SampleJsonData.SECRET);

		hppRequest.setCommentTwo("");

		try {
			ValidationUtils.validate(hppRequest);
		} catch (RealexValidationException ex) {
			Assert.fail("This HppRequest should have no validation errors.");
		}

		hppRequest
				.setCommentTwo("a-z A-Z 0-9 ' \", + “” ._ - & \\ / @ ! ? % ( )* : £ $ & € # [ ] | = ;ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷ø¤ùúûüýþÿŒŽšœžŸ¥");

		try {
			ValidationUtils.validate(hppRequest);
		} catch (RealexValidationException ex) {
			Assert.fail("This HppRequest should have no validation errors.");
		}
		char[] charsAtMax = new char[255];
		Arrays.fill(charsAtMax, '1');
		hppRequest.setCommentTwo(new String(charsAtMax));

		try {
			ValidationUtils.validate(hppRequest);
		} catch (RealexValidationException ex) {
			Assert.fail("This HppRequest should not have validation errors.");
		}

		char[] charsOverMax = new char[256];
		Arrays.fill(charsOverMax, 'a');
		hppRequest.setCommentTwo(new String(charsOverMax));

		try {
			ValidationUtils.validate(hppRequest);
			Assert.fail("This HppRequest should have validation errors.");
		} catch (RealexValidationException ex) {
			Assert.assertEquals(VALIDATION_MESSAGES.getString("hppRequest.comment2.size"), ex.getValidationMessages().get(0));
		}

	}

	/**
	 * Test return TSS flag.
	 */
	@Test
	public void returnTssFlagTest() {
		HppRequest hppRequest = SampleJsonData.generateValidHppRequest();
		hppRequest.generateDefaults(SampleJsonData.SECRET);

		hppRequest.setReturnTss("");

		try {
			ValidationUtils.validate(hppRequest);
		} catch (RealexValidationException ex) {
			Assert.fail("This HppRequest should have no validation errors.");
		}

		hppRequest.setReturnTss(null);

		try {
			ValidationUtils.validate(hppRequest);
		} catch (RealexValidationException ex) {
			Assert.fail("This HppRequest should have no validation errors.");
		}

		hppRequest.setReturnTss("11");

		try {
			ValidationUtils.validate(hppRequest);
			Assert.fail("This HppRequest should have validation errors.");
		} catch (RealexValidationException ex) {
			Assert.assertEquals(VALIDATION_MESSAGES.getString("hppRequest.returnTss.size"), ex.getValidationMessages().get(0));
		}

		hppRequest.setReturnTss("a");

		try {
			ValidationUtils.validate(hppRequest);
			Assert.fail("This HppRequest should have validation errors.");
		} catch (RealexValidationException ex) {
			Assert.assertEquals(VALIDATION_MESSAGES.getString("hppRequest.returnTss.pattern"), ex.getValidationMessages().get(0));
		}
	}

	/**
	 * Test shipping code.
	 */
	@Test
	public void shippingCodeTest() {
		HppRequest hppRequest = SampleJsonData.generateValidHppRequest();
		hppRequest.generateDefaults(SampleJsonData.SECRET);

		hppRequest.setShippingCode("");

		try {
			ValidationUtils.validate(hppRequest);
		} catch (RealexValidationException ex) {
			Assert.fail("This HppRequest should have no validation errors.");
		}

		hppRequest.setShippingCode("azAZ09,.-/|");

		try {
			ValidationUtils.validate(hppRequest);
		} catch (RealexValidationException ex) {
			Assert.fail("This HppRequest should have no validation errors.");
		}

		char[] charsAtMax = new char[30];
		Arrays.fill(charsAtMax, 'a');
		hppRequest.setShippingCode(new String(charsAtMax));

		try {
			ValidationUtils.validate(hppRequest);
		} catch (RealexValidationException ex) {
			Assert.fail("This HppRequest should not have validation errors.");
		}

		char[] charsOverMax = new char[31];
		Arrays.fill(charsOverMax, 'a');
		hppRequest.setShippingCode(new String(charsOverMax));

		try {
			ValidationUtils.validate(hppRequest);
			Assert.fail("This HppRequest should have validation errors.");
		} catch (RealexValidationException ex) {
			Assert.assertEquals(VALIDATION_MESSAGES.getString("hppRequest.shippingCode.size"), ex.getValidationMessages().get(0));
		}

		hppRequest.setShippingCode("+");

		try {
			ValidationUtils.validate(hppRequest);
			Assert.fail("This HppRequest should have validation errors.");
		} catch (RealexValidationException ex) {
			Assert.assertEquals(VALIDATION_MESSAGES.getString("hppRequest.shippingCode.pattern"), ex.getValidationMessages().get(0));
		}
	}

	/**
	 * Test shipping country.
	 */
	@Test
	public void shippingCountryTest() {
		HppRequest hppRequest = SampleJsonData.generateValidHppRequest();
		hppRequest.generateDefaults(SampleJsonData.SECRET);

		hppRequest.setShippingCountry("");

		try {
			ValidationUtils.validate(hppRequest);
		} catch (RealexValidationException ex) {
			Assert.fail("This HppRequest should have no validation errors.");
		}

		hppRequest.setShippingCountry("AZaz09,.-");

		try {
			ValidationUtils.validate(hppRequest);
		} catch (RealexValidationException ex) {
			Assert.fail("This HppRequest should have no validation errors.");
		}

		char[] charsAtMax = new char[50];
		Arrays.fill(charsAtMax, '1');
		hppRequest.setShippingCountry(new String(charsAtMax));

		try {
			ValidationUtils.validate(hppRequest);
		} catch (RealexValidationException ex) {
			Assert.fail("This HppRequest should not have validation errors.");
		}

		char[] charsOverMax = new char[51];
		Arrays.fill(charsOverMax, 'a');
		hppRequest.setShippingCountry(new String(charsOverMax));

		try {
			ValidationUtils.validate(hppRequest);
			Assert.fail("This HppRequest should have validation errors.");
		} catch (RealexValidationException ex) {
			Assert.assertEquals(VALIDATION_MESSAGES.getString("hppRequest.shippingCountry.size"), ex.getValidationMessages().get(0));
		}

		hppRequest.setShippingCountry("+");

		try {
			ValidationUtils.validate(hppRequest);
			Assert.fail("This HppRequest should have validation errors.");
		} catch (RealexValidationException ex) {
			Assert.assertEquals(VALIDATION_MESSAGES.getString("hppRequest.shippingCountry.pattern"), ex.getValidationMessages().get(0));
		}
	}

	/**
	 * Test billing code.  
	 */
	@Test
	public void billingCodeTest() {
		HppRequest hppRequest = SampleJsonData.generateValidHppRequest();
		hppRequest.generateDefaults(SampleJsonData.SECRET);

		hppRequest.setBillingCode("");

		try {
			ValidationUtils.validate(hppRequest);
		} catch (RealexValidationException ex) {
			Assert.fail("This HppRequest should have no validation errors.");
		}

		hppRequest.setBillingCode("azAZ09,.-/|");

		try {
			ValidationUtils.validate(hppRequest);
		} catch (RealexValidationException ex) {
			Assert.fail("This HppRequest should have no validation errors.");
		}

		char[] charsAtMax = new char[60];
		Arrays.fill(charsAtMax, '1');
		hppRequest.setBillingCode(new String(charsAtMax));

		try {
			ValidationUtils.validate(hppRequest);
		} catch (RealexValidationException ex) {
			Assert.fail("This HppRequest should not have validation errors.");
		}

		char[] charsOverMax = new char[61];
		Arrays.fill(charsOverMax, 'a');
		hppRequest.setBillingCode(new String(charsOverMax));

		try {
			ValidationUtils.validate(hppRequest);
			Assert.fail("This HppRequest should have validation errors.");
		} catch (RealexValidationException ex) {
			Assert.assertEquals(VALIDATION_MESSAGES.getString("hppRequest.billingCode.size"), ex.getValidationMessages().get(0));
		}

		hppRequest.setBillingCode("+");

		try {
			ValidationUtils.validate(hppRequest);
			Assert.fail("This HppRequest should have validation errors.");
		} catch (RealexValidationException ex) {
			Assert.assertEquals(VALIDATION_MESSAGES.getString("hppRequest.billingCode.pattern"), ex.getValidationMessages().get(0));
		}
	}

	/**
	 * Test billing country.
	 */
	@Test
	public void billingCountryTest() {
		HppRequest hppRequest = SampleJsonData.generateValidHppRequest();
		hppRequest.generateDefaults(SampleJsonData.SECRET);

		hppRequest.setBillingCountry("");

		try {
			ValidationUtils.validate(hppRequest);
		} catch (RealexValidationException ex) {
			Assert.fail("This HppRequest should have no validation errors.");
		}

		hppRequest.setBillingCountry("AZaz09,.-");

		try {
			ValidationUtils.validate(hppRequest);
		} catch (RealexValidationException ex) {
			Assert.fail("This HppRequest should have no validation errors.");
		}

		char[] charsAtMax = new char[50];
		Arrays.fill(charsAtMax, '1');
		hppRequest.setBillingCountry(new String(charsAtMax));

		try {
			ValidationUtils.validate(hppRequest);
		} catch (RealexValidationException ex) {
			Assert.fail("This HppRequest should not have validation errors.");
		}

		char[] charsOverMax = new char[51];
		Arrays.fill(charsOverMax, 'a');
		hppRequest.setBillingCountry(new String(charsOverMax));

		try {
			ValidationUtils.validate(hppRequest);
			Assert.fail("This HppRequest should have validation errors.");
		} catch (RealexValidationException ex) {
			Assert.assertEquals(VALIDATION_MESSAGES.getString("hppRequest.billingCountry.size"), ex.getValidationMessages().get(0));
		}

		hppRequest.setBillingCountry("+");

		try {
			ValidationUtils.validate(hppRequest);
			Assert.fail("This HppRequest should have validation errors.");
		} catch (RealexValidationException ex) {
			Assert.assertEquals(VALIDATION_MESSAGES.getString("hppRequest.billingCountry.pattern"), ex.getValidationMessages().get(0));
		}
	}

	/**
	 * Test customer number. 
	 */
	@Test
	public void customerNumberTest() {
		HppRequest hppRequest = SampleJsonData.generateValidHppRequest();
		hppRequest.generateDefaults(SampleJsonData.SECRET);

		hppRequest.setCustomerNumber("");

		try {
			ValidationUtils.validate(hppRequest);
		} catch (RealexValidationException ex) {
			Assert.fail("This HppRequest should have no validation errors.");
		}

		hppRequest.setCustomerNumber("az AZ 09 - _ . ,+ @ ");

		try {
			ValidationUtils.validate(hppRequest);
		} catch (RealexValidationException ex) {
			Assert.fail("This HppRequest should have no validation errors.");
		}

		char[] charsAtMax = new char[50];
		Arrays.fill(charsAtMax, '1');
		hppRequest.setCustomerNumber(new String(charsAtMax));

		try {
			ValidationUtils.validate(hppRequest);
		} catch (RealexValidationException ex) {
			Assert.fail("This HppRequest should not have validation errors.");
		}

		char[] charsOverMax = new char[51];
		Arrays.fill(charsOverMax, 'a');
		hppRequest.setCustomerNumber(new String(charsOverMax));

		try {
			ValidationUtils.validate(hppRequest);
			Assert.fail("This HppRequest should have validation errors.");
		} catch (RealexValidationException ex) {
			Assert.assertEquals(VALIDATION_MESSAGES.getString("hppRequest.customerNumber.size"), ex.getValidationMessages().get(0));
		}

		hppRequest.setCustomerNumber("&");

		try {
			ValidationUtils.validate(hppRequest);
			Assert.fail("This HppRequest should have validation errors.");
		} catch (RealexValidationException ex) {
			Assert.assertEquals(VALIDATION_MESSAGES.getString("hppRequest.customerNumber.pattern"), ex.getValidationMessages().get(0));
		}
	}

	/**
	 * Test variable reference. 
	 */
	@Test
	public void variableReferenceTest() {
		HppRequest hppRequest = SampleJsonData.generateValidHppRequest();
		hppRequest.generateDefaults(SampleJsonData.SECRET);

		hppRequest.setVariableReference("");

		try {
			ValidationUtils.validate(hppRequest);
		} catch (RealexValidationException ex) {
			Assert.fail("This HppRequest should have no validation errors.");
		}

		hppRequest.setVariableReference("az AZ 09 - _ . ,+ @ ");

		try {
			ValidationUtils.validate(hppRequest);
		} catch (RealexValidationException ex) {
			Assert.fail("This HppRequest should have no validation errors.");
		}

		char[] charsOverMax = new char[51];
		Arrays.fill(charsOverMax, 'a');
		hppRequest.setVariableReference(new String(charsOverMax));

		try {
			ValidationUtils.validate(hppRequest);
			Assert.fail("This HppRequest should have validation errors.");
		} catch (RealexValidationException ex) {
			Assert.assertEquals(VALIDATION_MESSAGES.getString("hppRequest.variableReference.size"), ex.getValidationMessages().get(0));
		}

		hppRequest.setVariableReference("&");

		try {
			ValidationUtils.validate(hppRequest);
			Assert.fail("This HppRequest should have validation errors.");
		} catch (RealexValidationException ex) {
			Assert.assertEquals(VALIDATION_MESSAGES.getString("hppRequest.variableReference.pattern"), ex.getValidationMessages().get(0));
		}
	}

	/**
	 * Test product ID.
	 */
	@Test
	public void productIdTest() {
		HppRequest hppRequest = SampleJsonData.generateValidHppRequest();
		hppRequest.generateDefaults(SampleJsonData.SECRET);

		hppRequest.setProductId("");

		try {
			ValidationUtils.validate(hppRequest);
		} catch (RealexValidationException ex) {
			Assert.fail("This HppRequest should have no validation errors.");
		}

		hppRequest.setProductId("az AZ 09 - _ . ,+ @ ");

		try {
			ValidationUtils.validate(hppRequest);
		} catch (RealexValidationException ex) {
			Assert.fail("This HppRequest should have no validation errors.");
		}

		char[] charsAtMax = new char[50];
		Arrays.fill(charsAtMax, '1');
		hppRequest.setProductId(new String(charsAtMax));

		try {
			ValidationUtils.validate(hppRequest);
		} catch (RealexValidationException ex) {
			Assert.fail("This HppRequest should not have validation errors.");
		}

		char[] charsOverMax = new char[51];
		Arrays.fill(charsOverMax, 'a');
		hppRequest.setProductId(new String(charsOverMax));

		try {
			ValidationUtils.validate(hppRequest);
			Assert.fail("This HppRequest should have validation errors.");
		} catch (RealexValidationException ex) {
			Assert.assertEquals(VALIDATION_MESSAGES.getString("hppRequest.productId.size"), ex.getValidationMessages().get(0));
		}

		hppRequest.setProductId("&");

		try {
			ValidationUtils.validate(hppRequest);
			Assert.fail("This HppRequest should have validation errors.");
		} catch (RealexValidationException ex) {
			Assert.assertEquals(VALIDATION_MESSAGES.getString("hppRequest.productId.pattern"), ex.getValidationMessages().get(0));
		}
	}

	/**
	 * Test language.
	 */
	@Test
	public void languageTest() {
		HppRequest hppRequest = SampleJsonData.generateValidHppRequest();
		hppRequest.generateDefaults(SampleJsonData.SECRET);

		hppRequest.setLanguage(null);

		try {
			ValidationUtils.validate(hppRequest);
		} catch (RealexValidationException ex) {
			Assert.fail("This HppRequest should have no validation errors.");
		}

		hppRequest.setLanguage("");

		try {
			ValidationUtils.validate(hppRequest);
		} catch (RealexValidationException ex) {
			Assert.fail("This HppRequest should have no validation errors.");
		}

		hppRequest.setLanguage("a");

		try {
			ValidationUtils.validate(hppRequest);
			Assert.fail("This HppRequest should have validation errors.");
		} catch (RealexValidationException ex) {
			Assert.assertEquals(VALIDATION_MESSAGES.getString("hppRequest.language.pattern"), ex.getValidationMessages().get(0));
		}

		hppRequest.setLanguage("abc");

		try {
			ValidationUtils.validate(hppRequest);
			Assert.fail("This HppRequest should have validation errors.");
		} catch (RealexValidationException ex) {
			Assert.assertEquals(VALIDATION_MESSAGES.getString("hppRequest.language.pattern"), ex.getValidationMessages().get(0));
		}

		hppRequest.setLanguage("%&");

		try {
			ValidationUtils.validate(hppRequest);
			Assert.fail("This HppRequest should have validation errors.");
		} catch (RealexValidationException ex) {
			Assert.assertEquals(VALIDATION_MESSAGES.getString("hppRequest.language.pattern"), ex.getValidationMessages().get(0));
		}
	}

	/**
	 * Test card payment button text.
	 */
	@Test
	public void cardPaymentButtonTextTest() {
		HppRequest hppRequest = SampleJsonData.generateValidHppRequest();
		hppRequest.generateDefaults(SampleJsonData.SECRET);

		hppRequest.setCardPaymentButtonText("");

		try {
			ValidationUtils.validate(hppRequest);
		} catch (RealexValidationException ex) {
			Assert.fail("This HppRequest should have no validation errors.");
		}

		hppRequest.setCardPaymentButtonText(" azAZ09'\",+“”._- & ");

		try {
			ValidationUtils.validate(hppRequest);
		} catch (RealexValidationException ex) {
			Assert.fail("This HppRequest should have no validation errors.");
		}

		hppRequest.setCardPaymentButtonText("\\/@!?%()*:£$&€#[]|");

		try {
			ValidationUtils.validate(hppRequest);
		} catch (RealexValidationException ex) {
			Assert.fail("This HppRequest should have no validation errors.");
		}

		hppRequest.setCardPaymentButtonText("=ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒ");

		try {
			ValidationUtils.validate(hppRequest);
		} catch (RealexValidationException ex) {
			Assert.fail("This HppRequest should have no validation errors.");
		}

		hppRequest.setCardPaymentButtonText("ÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæ");

		try {
			ValidationUtils.validate(hppRequest);
		} catch (RealexValidationException ex) {
			Assert.fail("This HppRequest should have no validation errors.");
		}

		hppRequest.setCardPaymentButtonText("çèéêëìíîïðñòóôõö÷ø¤ù");

		try {
			ValidationUtils.validate(hppRequest);
		} catch (RealexValidationException ex) {
			Assert.fail("This HppRequest should have no validation errors.");
		}

		hppRequest.setCardPaymentButtonText("úûüýþÿŒŽšœžŸ¥");

		try {
			ValidationUtils.validate(hppRequest);
		} catch (RealexValidationException ex) {
			Assert.fail("This HppRequest should have no validation errors.");
		}

		char[] charsAtMax = new char[25];
		Arrays.fill(charsAtMax, '1');
		hppRequest.setCardPaymentButtonText(new String(charsAtMax));

		try {
			ValidationUtils.validate(hppRequest);
		} catch (RealexValidationException ex) {
			Assert.fail("This HppRequest should not have validation errors.");
		}

		char[] charsOverMax = new char[26];
		Arrays.fill(charsOverMax, 'a');
		hppRequest.setCardPaymentButtonText(new String(charsOverMax));

		try {
			ValidationUtils.validate(hppRequest);
			Assert.fail("This HppRequest should have validation errors.");
		} catch (RealexValidationException ex) {
			Assert.assertEquals(VALIDATION_MESSAGES.getString("hppRequest.cardPaymentButtonText.size"), ex.getValidationMessages().get(0));
		}
	}

	/**
	 * Test card storage enable. 
	 */
	@Test
	public void cardStorageEnableTest() {
		HppRequest hppRequest = SampleJsonData.generateValidHppRequest();
		hppRequest.generateDefaults(SampleJsonData.SECRET);

		hppRequest.setCardStorageEnable("");

		try {
			ValidationUtils.validate(hppRequest);
		} catch (RealexValidationException ex) {
			Assert.fail("This HppRequest should have no validation errors.");
		}

		hppRequest.setCardStorageEnable("11");

		try {
			ValidationUtils.validate(hppRequest);
			Assert.fail("This HppRequest should have validation errors.");
		} catch (RealexValidationException ex) {
			Assert.assertEquals(VALIDATION_MESSAGES.getString("hppRequest.cardStorageEnable.size"), ex.getValidationMessages().get(0));
		}

		hppRequest.setCardStorageEnable("a");

		try {
			ValidationUtils.validate(hppRequest);
			Assert.fail("This HppRequest should have validation errors.");
		} catch (RealexValidationException ex) {
			Assert.assertEquals(VALIDATION_MESSAGES.getString("hppRequest.cardStorageEnable.pattern"), ex.getValidationMessages().get(0));
		}
	}

	/**
	 * Test offer save card. 
	 */
	@Test
	public void offerSaveCardTest() {
		HppRequest hppRequest = SampleJsonData.generateValidHppRequest();
		hppRequest.generateDefaults(SampleJsonData.SECRET);

		hppRequest.setOfferSaveCard("");

		try {
			ValidationUtils.validate(hppRequest);
		} catch (RealexValidationException ex) {
			Assert.fail("This HppRequest should have no validation errors.");
		}

		hppRequest.setOfferSaveCard("1");

		try {
			ValidationUtils.validate(hppRequest);
		} catch (RealexValidationException ex) {
			Assert.fail("This HppRequest should have no validation errors.");
		}

		hppRequest.setOfferSaveCard("0");

		try {
			ValidationUtils.validate(hppRequest);
		} catch (RealexValidationException ex) {
			Assert.fail("This HppRequest should have no validation errors.");
		}

		hppRequest.setOfferSaveCard("11");

		try {
			ValidationUtils.validate(hppRequest);
			Assert.fail("This HppRequest should have validation errors.");
		} catch (RealexValidationException ex) {
			Assert.assertEquals(VALIDATION_MESSAGES.getString("hppRequest.offerSaveCard.size"), ex.getValidationMessages().get(0));
		}

		hppRequest.setOfferSaveCard("a");

		try {
			ValidationUtils.validate(hppRequest);
			Assert.fail("This HppRequest should have validation errors.");
		} catch (RealexValidationException ex) {
			Assert.assertEquals(VALIDATION_MESSAGES.getString("hppRequest.offerSaveCard.pattern"), ex.getValidationMessages().get(0));
		}
	}

	/**
	 * Test payer reference. 
	 */
	@Test
	public void payerReferenceTest() {
		HppRequest hppRequest = SampleJsonData.generateValidHppRequest();
		hppRequest.generateDefaults(SampleJsonData.SECRET);

		hppRequest.setPayerReference("");

		try {
			ValidationUtils.validate(hppRequest);
		} catch (RealexValidationException ex) {
			Assert.fail("This HppRequest should have no validation errors.");
		}

		hppRequest.setPayerReference("azAZ09\\ _");

		try {
			ValidationUtils.validate(hppRequest);
		} catch (RealexValidationException ex) {
			Assert.fail("This HppRequest should have no validation errors.");
		}

		char[] charsAtMax = new char[50];
		Arrays.fill(charsAtMax, '1');
		hppRequest.setPayerReference(new String(charsAtMax));

		try {
			ValidationUtils.validate(hppRequest);
		} catch (RealexValidationException ex) {
			Assert.fail("This HppRequest should not have validation errors.");
		}

		char[] charsOverMax = new char[51];
		Arrays.fill(charsOverMax, 'a');
		hppRequest.setPayerReference(new String(charsOverMax));

		try {
			ValidationUtils.validate(hppRequest);
			Assert.fail("This HppRequest should have validation errors.");
		} catch (RealexValidationException ex) {
			Assert.assertEquals(VALIDATION_MESSAGES.getString("hppRequest.payerReference.size"), ex.getValidationMessages().get(0));
		}

		hppRequest.setPayerReference("+");

		try {
			ValidationUtils.validate(hppRequest);
			Assert.fail("This HppRequest should have validation errors.");
		} catch (RealexValidationException ex) {
			Assert.assertEquals(VALIDATION_MESSAGES.getString("hppRequest.payerReference.pattern"), ex.getValidationMessages().get(0));
		}
	}

	/**
	 * Test payment reference. 
	 */
	@Test
	public void paymentReferenceTest() {
		HppRequest hppRequest = SampleJsonData.generateValidHppRequest();
		hppRequest.generateDefaults(SampleJsonData.SECRET);

		hppRequest.setPaymentReference("");

		try {
			ValidationUtils.validate(hppRequest);
		} catch (RealexValidationException ex) {
			Assert.fail("This HppRequest should have no validation errors.");
		}

		hppRequest.setPaymentReference("azAZ09-_");

		try {
			ValidationUtils.validate(hppRequest);
		} catch (RealexValidationException ex) {
			Assert.fail("This HppRequest should have no validation errors.");
		}

		char[] charsAtMax = new char[50];
		Arrays.fill(charsAtMax, '1');
		hppRequest.setPaymentReference(new String(charsAtMax));

		try {
			ValidationUtils.validate(hppRequest);
		} catch (RealexValidationException ex) {
			Assert.fail("This HppRequest should not have validation errors.");
		}

		char[] charsOverMax = new char[51];
		Arrays.fill(charsOverMax, 'a');
		hppRequest.setPaymentReference(new String(charsOverMax));

		try {
			ValidationUtils.validate(hppRequest);
			Assert.fail("This HppRequest should have validation errors.");
		} catch (RealexValidationException ex) {
			Assert.assertEquals(VALIDATION_MESSAGES.getString("hppRequest.paymentReference.size"), ex.getValidationMessages().get(0));
		}

		hppRequest.setPaymentReference("+");

		try {
			ValidationUtils.validate(hppRequest);
			Assert.fail("This HppRequest should have validation errors.");
		} catch (RealexValidationException ex) {
			Assert.assertEquals(VALIDATION_MESSAGES.getString("hppRequest.paymentReference.pattern"), ex.getValidationMessages().get(0));
		}
	}

	/**
	 * Test payer exists. 
	 */
	@Test
	public void payerExistsTest() {
		HppRequest hppRequest = SampleJsonData.generateValidHppRequest();
		hppRequest.generateDefaults(SampleJsonData.SECRET);

		hppRequest.setPayerExists("");

		try {
			ValidationUtils.validate(hppRequest);
		} catch (RealexValidationException ex) {
			Assert.fail("This HppRequest should have no validation errors.");
		}

		hppRequest.setPayerExists("0");

		try {
			ValidationUtils.validate(hppRequest);
		} catch (RealexValidationException ex) {
			Assert.fail("This HppRequest should have no validation errors.");
		}

		hppRequest.setPayerExists("1");

		try {
			ValidationUtils.validate(hppRequest);
		} catch (RealexValidationException ex) {
			Assert.fail("This HppRequest should have no validation errors.");
		}

		hppRequest.setPayerExists("2");

		try {
			ValidationUtils.validate(hppRequest);
		} catch (RealexValidationException ex) {
			Assert.fail("This HppRequest should have no validation errors.");
		}

		hppRequest.setPayerExists("11");

		try {
			ValidationUtils.validate(hppRequest);
			Assert.fail("This HppRequest should have validation errors.");
		} catch (RealexValidationException ex) {
			Assert.assertEquals(VALIDATION_MESSAGES.getString("hppRequest.payerExists.size"), ex.getValidationMessages().get(0));
		}

		hppRequest.setPayerExists("a");

		try {
			ValidationUtils.validate(hppRequest);
			Assert.fail("This HppRequest should have validation errors.");
		} catch (RealexValidationException ex) {
			Assert.assertEquals(VALIDATION_MESSAGES.getString("hppRequest.payerExists.pattern"), ex.getValidationMessages().get(0));
		}
	}

	/**
	 * Test validate card only. 
	 */
	@Test
	public void validateCardOnlyTest() {
		HppRequest hppRequest = SampleJsonData.generateValidHppRequest();
		hppRequest.generateDefaults(SampleJsonData.SECRET);

		hppRequest.setValidateCardOnly("");

		try {
			ValidationUtils.validate(hppRequest);
		} catch (RealexValidationException ex) {
			Assert.fail("This HppRequest should have no validation errors.");
		}

		hppRequest.setValidateCardOnly("0");

		try {
			ValidationUtils.validate(hppRequest);
		} catch (RealexValidationException ex) {
			Assert.fail("This HppRequest should have no validation errors.");
		}

		hppRequest.setValidateCardOnly("11");

		try {
			ValidationUtils.validate(hppRequest);
			Assert.fail("This HppRequest should have validation errors.");
		} catch (RealexValidationException ex) {
			Assert.assertEquals(VALIDATION_MESSAGES.getString("hppRequest.validateCardOnly.size"), ex.getValidationMessages().get(0));
		}

		hppRequest.setValidateCardOnly("a");

		try {
			ValidationUtils.validate(hppRequest);
			Assert.fail("This HppRequest should have validation errors.");
		} catch (RealexValidationException ex) {
			Assert.assertEquals(VALIDATION_MESSAGES.getString("hppRequest.validateCardOnly.pattern"), ex.getValidationMessages().get(0));
		}

		hppRequest.setValidateCardOnly("1");
		hppRequest.setAmount("0");

		try {
			ValidationUtils.validate(hppRequest);
		} catch (RealexValidationException ex) {
			Assert.fail("This HppRequest should have no validation errors.");
		}
	}

	/**
	 * Test DCC enable. 
	 */
	@Test
	public void dccEnableTest() {
		HppRequest hppRequest = SampleJsonData.generateValidHppRequest();
		hppRequest.generateDefaults(SampleJsonData.SECRET);

		hppRequest.setDccEnable("");

		try {
			ValidationUtils.validate(hppRequest);
		} catch (RealexValidationException ex) {
			Assert.fail("This HppRequest should have no validation errors.");
		}

		hppRequest.setDccEnable("0");

		try {
			ValidationUtils.validate(hppRequest);
		} catch (RealexValidationException ex) {
			Assert.fail("This HppRequest should have no validation errors.");
		}

		hppRequest.setDccEnable("11");

		try {
			ValidationUtils.validate(hppRequest);
			Assert.fail("This HppRequest should have validation errors.");
		} catch (RealexValidationException ex) {
			Assert.assertEquals(VALIDATION_MESSAGES.getString("hppRequest.dccEnable.size"), ex.getValidationMessages().get(0));
		}

		hppRequest.setDccEnable("a");

		try {
			ValidationUtils.validate(hppRequest);
			Assert.fail("This HppRequest should have validation errors.");
		} catch (RealexValidationException ex) {
			Assert.assertEquals(VALIDATION_MESSAGES.getString("hppRequest.dccEnable.pattern"), ex.getValidationMessages().get(0));
		}

	}

}
