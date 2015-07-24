/*
 * Classname : HppRequestTest.java
 *
 * Created on: 24 Jul 2015
 *
 * Copyright (c) 2000-2015 Realex Payments, Ltd.
 * Realex Payments, The Observatory, 7-11 Sir John Rogerson's Quay, Dublin 2, Ireland
 *  
 * All Rights Reserved.
 *
 * This software is the confidential and proprietary information of
 * Realex Payments, Ltd. ("Confidential Information"). You shall not
 * disclose such Confidential Information and shall use it only in
 * accordance with the terms of the license agreement you entered into
 * with Realex Payments.
 *
 */
package com.realexpayments.hpp.sdk.domain;

import org.junit.Assert;
import org.junit.Test;

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
}
