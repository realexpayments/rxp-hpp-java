package com.realexpayments.hpp.sdk;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.UnsupportedEncodingException;
import java.util.Scanner;

import org.junit.Test;

import com.realexpayments.hpp.sdk.domain.HppRequest;
import com.realexpayments.hpp.sdk.domain.HppResponse;

/**
 * Unit test class for {@link RealexHpp}  
 * 
 * @author markstanford
 *
 */
public class RealexHppTest {

	/**
	 * {@link RealexHpp} singleton.
	 */
	private static final RealexHpp REALEX_HPP = new RealexHpp(SampleJsonData.SECRET);

	/**
	 * Test converting a {@link HppRequest} object to JSON. Includes validation and generation of defaults. 
	 * @throws UnsupportedEncodingException 
	 */
	@Test
	public void requestToJsonSuccessTest() throws UnsupportedEncodingException {
		HppRequest hppRequestExpected = SampleJsonData.generateValidHppRequestWithEmptyDefaults(false);
		String json = REALEX_HPP.requestToJson(hppRequestExpected);

		HppRequest hppRequestConverted = REALEX_HPP.requestFromJson(json);
		hppRequestExpected.decode(RealexHpp.ENCODING_CHARSET);
		SampleJsonData.checkValidHppRequest(hppRequestExpected, hppRequestConverted, false);
		SampleJsonData.checkValidHppRequestSupplementaryData(hppRequestConverted);
	}

	/**
	 * Test converting encoded JSON to {@link HppRequest}.
	 */
	@Test
	public void requestFromJsonEncodedSuccessTest() throws FileNotFoundException {
		HppRequest hppRequestExpected = SampleJsonData.generateValidHppRequest(false);
		File file = new File(this.getClass().getResource(SampleJsonData.VALID_HPP_REQUEST_ENCODED_JSON_PATH).getPath());
		String json = new Scanner(file, RealexHpp.ENCODING_CHARSET).useDelimiter("\\A").next();

		HppRequest hppRequestConverted = REALEX_HPP.requestFromJson(json);
		SampleJsonData.checkValidHppRequest(hppRequestExpected, hppRequestConverted, false);
	}

	/**
	 * Test converting unencoded JON to {@link HppRequest}.
	 * 
	 * @throws FileNotFoundException
	 */
	@Test
	public void requestFromJsonDecodedSuccessTest() throws FileNotFoundException {
		HppRequest hppRequestExpected = SampleJsonData.generateValidHppRequest(false);
		File file = new File(this.getClass().getResource(SampleJsonData.VALID_HPP_REQUEST_JSON_PATH).getPath());
		String json = new Scanner(file, RealexHpp.ENCODING_CHARSET).useDelimiter("\\A").next();

		HppRequest hppRequestConverted = REALEX_HPP.requestFromJson(json, false);
		SampleJsonData.checkValidHppRequest(hppRequestExpected, hppRequestConverted, false);
	}

	/**
	 * Test converting card storage encoded JSON to {@link HppRequest}.
	 */
	@Test
	public void requestFromJsonCardStorageSuccessTest() throws FileNotFoundException {
		HppRequest hppRequestExpected = SampleJsonData.generateValidHppRequest(true);
		File file = new File(this.getClass().getResource(SampleJsonData.VALID_HPP_REQUEST_CARD_STORAGE_JSON_PATH).getPath());
		String json = new Scanner(file, RealexHpp.ENCODING_CHARSET).useDelimiter("\\A").next();

		HppRequest hppRequestConverted = REALEX_HPP.requestFromJson(json, false);
		SampleJsonData.checkValidHppRequest(hppRequestExpected, hppRequestConverted, false);
	}

	/**
	 * Test converting {@link HppResponse} to JSON.  Includes hash validation.
	 * @throws UnsupportedEncodingException 
	 */
	@Test
	public void responseToJsonSuccessTest() throws UnsupportedEncodingException {
		HppResponse hppResponseExpected = SampleJsonData.generateValidHppResponse();
		String json = REALEX_HPP.responseToJson(hppResponseExpected);

		HppResponse hppResponseConverted = REALEX_HPP.responseFromJson(json);
		hppResponseExpected.decode(RealexHpp.ENCODING_CHARSET);
		SampleJsonData.checkValidHppResponse(hppResponseExpected, hppResponseConverted);
		SampleJsonData.checkValidHppResponseSupplementaryData(hppResponseConverted);
	}

	/**
	 * Test converting encoded JSON to {@link HppResponse}.
	 * 
	 * @throws FileNotFoundException
	 */
	@Test
	public void responseFromJsonEncodedSuccessTest() throws FileNotFoundException {
		HppResponse hppResponseExpected = SampleJsonData.generateValidHppResponse();
		File file = new File(this.getClass().getResource(SampleJsonData.VALID_HPP_RESPONSE_ENCODED_JSON_PATH).getPath());
		String json = new Scanner(file, RealexHpp.ENCODING_CHARSET).useDelimiter("\\A").next();

		HppResponse hppResponseConverted = REALEX_HPP.responseFromJson(json);
		SampleJsonData.checkValidHppResponse(hppResponseExpected, hppResponseConverted);
		SampleJsonData.checkValidHppResponseSupplementaryData(hppResponseConverted);
	}

	/**
	 * Test converting unencoded JSON to {@link HppResponse}.
	 * 
	 * @throws FileNotFoundException
	 */
	@Test
	public void responseFromJsonDecodedSuccessTest() throws FileNotFoundException {
		HppResponse hppResponseExpected = SampleJsonData.generateValidHppResponse();
		File file = new File(this.getClass().getResource(SampleJsonData.VALID_HPP_RESPONSE_JSON_PATH).getPath());
		String json = new Scanner(file, RealexHpp.ENCODING_CHARSET).useDelimiter("\\A").next();

		HppResponse hppResponseConverted = REALEX_HPP.responseFromJson(json, false);
		SampleJsonData.checkValidHppResponse(hppResponseExpected, hppResponseConverted);
	}

}
