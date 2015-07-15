package com.realexpayments.hpp.sdk;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.realexpayments.hpp.sdk.domain.HppRequest;
import com.realexpayments.hpp.sdk.domain.HppResponse;
import com.realexpayments.hpp.sdk.utils.JsonUtils;
import com.realexpayments.hpp.sdk.utils.ValidationUtils;

/**
 * <p>
 * RealexHpp class for converting HPP requests and responses to and from JSON. 
 * This class is also responsible for validating inputs, generating defaults and encoding parameter values.  
 * </p>
 * <p>
 * Creating Request JSON for Realex JS SDK
 * <code><pre>
 * HppRequest hppRequest = new HppRequest().addMerchantId("merchantId").addAmount(100)...addAutoSettleFlag(true);
 * RealexHpp realexHpp = new RealexHpp("mySecret");
 * String json = realexHpp.requestToJson(hppRequest);
 * </pre></code>
 * </p>
 * <p>
 * Consuming Response JSON from Realex JS SDK
 * <code><pre>
 * RealexHpp realexHpp = new RealexHpp("mySecret");
 * HppResponse hppResponse = realexHpp.responseFromJson(responseJson); 
 * </pre></code>
 * </p>
 * @author markstanford
 *
 */
public class RealexHpp {

	/**
	 * Logger 
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(RealexHpp.class);

	/**
	 * The shared secret issued by Realex. Used to create the SHA-1 hash in the request and
	 * to verify the validity of the XML response. 
	 */
	private String secret;

	/**
	 * RealexHpp constructor. 
	 * 
	 * @param secret
	 */
	public RealexHpp(String secret) {
		this.secret = secret;
	}

	/**
	 * <p>
	 * Method produces JSON from <code>HppRequest</code> object. 
	 * Carries out the following actions:
	 * <ul>
	 * <li>Validates inputs</li>
	 * <li>Generates defaults for security hash, order ID and time stamp (if required)</li>
	 * <li>Base64 encodes inputs</li>
	 * <li>Serialises request object to JSON</li>
	 * </ul>
	 * </p>
	 * 
	 * @param hppRequest
	 * @return String
	 */
	public String requestToJson(HppRequest hppRequest) {

		LOGGER.info("Converting HppRequest to JSON.");

		String json = null;

		//validate request
		LOGGER.debug("Validating request.");
		ValidationUtils.validate(hppRequest);

		//generate defaults
		LOGGER.debug("Generating defaults.");
		hppRequest.generateDefaults(secret);

		//encode 
		LOGGER.debug("Encoding object.");
		hppRequest = hppRequest.encode();

		//convert to JSON
		LOGGER.debug("Converting to JSON.");
		json = JsonUtils.toJson(hppRequest);

		return json;
	}

	/**
	 * <p>
	 * Method produces <code>HppRequest</code> object from JSON. 
	 * Carries out the following actions:
	 * <ul>
	 * <li>Deserialises JSON to request object</li>
	 * <li>Decodes Base64 inputs</li>
	 * <li>Validates inputs</li>
	 * </ul>
	 * </p>
	 * 
	 * @param json
	 * @param encoded <code>true</code> if the JSON values have been encoded.
	 * @return HppRequest
	 */
	public HppRequest requestFromJson(String json, boolean encoded) {

		LOGGER.info("Converting JSON to HppRequest.");

		//convert to HppRequest from JSON
		HppRequest hppRequest = JsonUtils.fromJsonHppRequest(json);

		//decode if necessary
		if (encoded) {
			LOGGER.debug("Decoding object.");
			hppRequest = hppRequest.decode();
		}

		//validate HPP request 
		LOGGER.debug("Validating request.");
		ValidationUtils.validate(hppRequest);

		return hppRequest;
	}

	/**
	 * <p>
	 * Method produces <code>HppRequest</code> object from JSON. 
	 * Carries out the following actions:
	 * <ul>
	 * <li>Deserialises JSON to request object</li>
	 * <li>Decodes Base64 inputs</li>
	 * <li>Validates inputs</li>
	 * </ul>
	 * </p> 
	 * 
	 * @param json
	 * @return HppRequest
	 */
	public HppRequest requestFromJson(String json) {
		return requestFromJson(json, true);
	}

	/**
	 * <p>
	 * Method produces JSON from <code>HppResponse</code> object. 
	 * Carries out the following actions:
	 * <ul>
	 * <li>Generates security hash</li>
	 * <li>Base64 encodes inputs</li>
	 * <li>Serialises response object to JSON</li>
	 * </ul>
	 * </p>
	 * 
	 * @param hppResponse
	 * @return String
	 */
	public String responseToJson(HppResponse hppResponse) {

		LOGGER.info("Converting HppResponse to JSON.");

		String json = null;

		//generate hash
		LOGGER.debug("Generating hash.");
		hppResponse.hash(secret);

		//encode 
		LOGGER.debug("Encoding object.");
		hppResponse = hppResponse.encode();

		//convert to JSON
		LOGGER.debug("Converting to JSON.");
		json = JsonUtils.toJson(hppResponse);

		return json;
	}

	/**
	 * <p>
	 * Method produces <code>HppResponse</code> object from JSON. 
	 * Carries out the following actions:
	 * <ul>
	 * <li>Deserialises JSON to response object</li>
	 * <li>Decodes Base64 inputs</li>
	 * <li>Validates hash</li>
	 * </ul>
	 * </p> 
	 * 
	 * @param json
	 * @return HppRequest
	 */
	public HppResponse responseFromJson(String json) {
		return responseFromJson(json, true);
	}

	/**
	 * <p>
	 * Method produces <code>HppResponse</code> object from JSON. 
	 * Carries out the following actions:
	 * <ul>
	 * <li>Deserialises JSON to response object</li>
	 * <li>Decodes Base64 inputs</li>
	 * <li>Validates hash</li>
	 * </ul>
	 * </p> 
	 * 
	 * @param json
	 * @param encoded <code>true</code> if the JSON values have been encoded.
	 * @return HppRequest
	 */
	public HppResponse responseFromJson(String json, boolean encoded) {

		LOGGER.info("Converting JSON to HppResponse.");

		//convert to HppResponse from JSON
		HppResponse hppResponse = JsonUtils.fromJsonHppResponse(json);

		//decode if necessary
		if (encoded) {
			LOGGER.debug("Decoding object.");
			hppResponse = hppResponse.decode();
		}

		//validate HPP response hash 
		LOGGER.debug("Validating response hash.");
		ValidationUtils.validate(hppResponse, secret);

		return hppResponse;
	}

}
