package com.realexpayments.hpp.sdk;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

import org.junit.Test;

import com.realexpayments.hpp.sdk.domain.HppRequest;
import com.realexpayments.hpp.sdk.domain.HppResponse;

/**
 * @author markstanford
 *
 */
public class RealexHppTest {

	private static final RealexHpp REALEX_HPP = new RealexHpp(SampleJsonData.SECRET);

	@Test
	public void requestToJsonSuccessTest() {
		HppRequest hppRequestExpected = SampleJsonData.generateValidHppRequestWithEmptyDefaults();
		String json = REALEX_HPP.requestToJson(hppRequestExpected);

		HppRequest hppRequestConverted = REALEX_HPP.requestFromJson(json);
		hppRequestExpected.decode();
		SampleJsonData.checkValidHppRequest(hppRequestExpected, hppRequestConverted, false);
		SampleJsonData.checkValidHppRequestSupplementaryData(hppRequestConverted);
	}

	@Test
	public void requestFromJsonEncodedSuccessTest() throws FileNotFoundException {
		HppRequest hppRequestExpected = SampleJsonData.generateValidHppRequest();
		File file = new File(this.getClass().getResource(SampleJsonData.VALID_HPP_REQUEST_ENCODED_JSON_PATH).getPath());
		String json = new Scanner(file).useDelimiter("\\A").next();

		HppRequest hppRequestConverted = REALEX_HPP.requestFromJson(json);
		SampleJsonData.checkValidHppRequest(hppRequestExpected, hppRequestConverted, false);
	}

	@Test
	public void requestFromJsonDecodedSuccessTest() throws FileNotFoundException {
		HppRequest hppRequestExpected = SampleJsonData.generateValidHppRequest();
		File file = new File(this.getClass().getResource(SampleJsonData.VALID_HPP_REQUEST_JSON_PATH).getPath());
		String json = new Scanner(file).useDelimiter("\\A").next();

		HppRequest hppRequestConverted = REALEX_HPP.requestFromJson(json, false);
		SampleJsonData.checkValidHppRequest(hppRequestExpected, hppRequestConverted, false);
	}

	@Test
	public void responseToJsonSuccessTest() {
		HppResponse hppResponseExpected = SampleJsonData.generateValidHppResponse();
		String json = REALEX_HPP.responseToJson(hppResponseExpected);

		HppResponse hppResponseConverted = REALEX_HPP.responseFromJson(json);
		hppResponseExpected.decode();
		SampleJsonData.checkValidHppResponse(hppResponseExpected, hppResponseConverted);
		SampleJsonData.checkValidHppResponseSupplementaryData(hppResponseConverted);
	}

	@Test
	public void responseFromJsonEncodedSuccessTest() throws FileNotFoundException {
		HppResponse hppResponseExpected = SampleJsonData.generateValidHppResponse();
		File file = new File(this.getClass().getResource(SampleJsonData.VALID_HPP_RESPONSE_ENCODED_JSON_PATH).getPath());
		String json = new Scanner(file).useDelimiter("\\A").next();

		HppResponse hppResponseConverted = REALEX_HPP.responseFromJson(json);
		SampleJsonData.checkValidHppResponse(hppResponseExpected, hppResponseConverted);
		SampleJsonData.checkValidHppResponseSupplementaryData(hppResponseConverted);
	}

	@Test
	public void responseFromJsonDecodedSuccessTest() throws FileNotFoundException {
		HppResponse hppResponseExpected = SampleJsonData.generateValidHppResponse();
		File file = new File(this.getClass().getResource(SampleJsonData.VALID_HPP_RESPONSE_JSON_PATH).getPath());
		String json = new Scanner(file).useDelimiter("\\A").next();

		HppResponse hppResponseConverted = REALEX_HPP.responseFromJson(json, false);
		SampleJsonData.checkValidHppResponse(hppResponseExpected, hppResponseConverted);
	}

}
