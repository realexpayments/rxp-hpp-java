package com.realexpayments.hpp.sdk.utils;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

import org.junit.Test;

import com.realexpayments.hpp.sdk.SampleJsonData;
import com.realexpayments.hpp.sdk.domain.HppRequest;
import com.realexpayments.hpp.sdk.domain.HppResponse;

/**
 * @author markstanford
 *
 */
public class JsonUtilsTest {

	@Test
	public void toJsonHppRequestTest() {
		HppRequest hppRequestExpected = SampleJsonData.generateValidHppRequest();
		String json = JsonUtils.toJson(hppRequestExpected);
		HppRequest hppRequestConverted = JsonUtils.fromJsonHppRequest(json);
		SampleJsonData.checkValidHppRequest(hppRequestExpected, hppRequestConverted, true);
		SampleJsonData.checkValidHppRequestSupplementaryData(hppRequestConverted);
	}

	@Test
	public void fromJsonHppRequestTest() throws FileNotFoundException {
		File file = new File(this.getClass().getResource(SampleJsonData.VALID_HPP_REQUEST_JSON_PATH).getPath());
		String json = new Scanner(file).useDelimiter("\\A").next();
		HppRequest hppRequestExpected = SampleJsonData.generateValidHppRequest();
		HppRequest hppRequestConverted = JsonUtils.fromJsonHppRequest(json);
		SampleJsonData.checkValidHppRequest(hppRequestExpected, hppRequestConverted, true);
	}

	@Test
	public void fromJsonHppRequestUnknownDataTest() throws FileNotFoundException {
		File file = new File(this.getClass().getResource(SampleJsonData.UNKNOWN_DATA_HPP_REQUEST_JSON_PATH).getPath());
		String json = new Scanner(file).useDelimiter("\\A").next();
		HppRequest hppRequestExpected = SampleJsonData.generateValidHppRequest();
		HppRequest hppRequestConverted = JsonUtils.fromJsonHppRequest(json);
		SampleJsonData.checkValidHppRequest(hppRequestExpected, hppRequestConverted, true);
		SampleJsonData.checkValidHppRequestSupplementaryData(hppRequestConverted);
	}

	@Test
	public void toJsonHppResponseTest() {
		HppResponse hppResponseExpected = SampleJsonData.generateValidHppResponse();
		String json = JsonUtils.toJson(hppResponseExpected);
		HppResponse hppResponseConverted = JsonUtils.fromJsonHppResponse(json);
		SampleJsonData.checkValidHppResponse(hppResponseExpected, hppResponseConverted);
	}

	@Test
	public void fromJsonHppResponseTest() throws FileNotFoundException {
		File file = new File(this.getClass().getResource(SampleJsonData.VALID_HPP_RESPONSE_JSON_PATH).getPath());
		String json = new Scanner(file).useDelimiter("\\A").next();
		HppResponse hppResponseExpected = SampleJsonData.generateValidHppResponse();
		HppResponse hppResponseConverted = JsonUtils.fromJsonHppResponse(json);
		SampleJsonData.checkValidHppResponse(hppResponseExpected, hppResponseConverted);
	}

	@Test
	public void fromJsonHppResponseUnknownDataTest() throws FileNotFoundException {
		File file = new File(this.getClass().getResource(SampleJsonData.UNKNOWN_DATA_HPP_RESPONSE_JSON_PATH).getPath());
		String json = new Scanner(file).useDelimiter("\\A").next();
		HppResponse hppResponseExpected = SampleJsonData.generateValidHppResponse();
		HppResponse hppResponseConverted = JsonUtils.fromJsonHppResponse(json);
		SampleJsonData.checkValidHppResponse(hppResponseExpected, hppResponseConverted);
		SampleJsonData.checkValidHppResponseSupplementaryData(hppResponseConverted);
	}
}
