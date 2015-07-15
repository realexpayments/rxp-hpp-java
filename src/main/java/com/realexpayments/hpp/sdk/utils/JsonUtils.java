package com.realexpayments.hpp.sdk.utils;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.ObjectReader;
import com.fasterxml.jackson.databind.ObjectWriter;
import com.realexpayments.hpp.sdk.RealexException;
import com.realexpayments.hpp.sdk.domain.HppRequest;
import com.realexpayments.hpp.sdk.domain.HppResponse;

/**
 * Class serialises and deserialises HPP request and response objects to amd from JSON.  
 * 
 * @author markstanford
 */
public class JsonUtils {

	/**
	 * Logger.
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(JsonUtils.class);

	/**
	 * HPP Request object reader. 
	 */
	private static ObjectReader hppRequestReader;

	/**
	 * HPP Response object reader. 
	 */
	private static ObjectReader hppResponseReader;

	/**
	 * HPP Request object writer. 
	 */
	private static ObjectWriter hppRequestWriter;

	/**
	 * HPP Response object writer. 
	 */
	private static ObjectWriter hppResponseWriter;

	/**
	 * Static initialiser creates object reader and writer instances.
	 */
	static {
		ObjectMapper mapper = new ObjectMapper();
		hppRequestReader = mapper.reader(HppRequest.class);
		hppResponseReader = mapper.reader(HppResponse.class);
		hppRequestWriter = mapper.writerFor(HppRequest.class);
		hppResponseWriter = mapper.writerFor(HppResponse.class);
	}

	/**
	 * Method serialises <code>HppRequest</code> to JSON.
	 * 
	 * @param hppRequest
	 * @return String
	 */
	public static String toJson(HppRequest hppRequest) {
		try {
			return hppRequestWriter.writeValueAsString(hppRequest);
		} catch (JsonProcessingException ex) {
			LOGGER.error("Error writing HppRequest to JSON.", ex);
			throw new RealexException("Error writing HppRequest to JSON.", ex);
		}
	}

	/**
	 * Method serialises <code>HppResponse</code> to JSON.
	 * 
	 * @param hppResponse
	 * @return String
	 */
	public static String toJson(HppResponse hppResponse) {
		try {
			return hppResponseWriter.writeValueAsString(hppResponse);
		} catch (JsonProcessingException ex) {
			LOGGER.error("Error writing HppResponse to JSON.", ex);
			throw new RealexException("Error writing HppResponse to JSON.", ex);
		}
	}

	/**
	 * Method deserialises JSON to <code>HppRequest</code>.
	 * 
	 * @param json
	 * @return HppRequest
	 */
	public static HppRequest fromJsonHppRequest(String json) {
		try {
			return hppRequestReader.readValue(json);
		} catch (Exception ex) {
			LOGGER.error("Error creating HppRequest from JSON.", ex);
			throw new RealexException("Error creating HppRequest from JSON.", ex);
		}
	}

	/**
	 * Method deserialises JSON to <code>HppResponse</code>.
	 * 
	 * @param json
	 * @return HppResponse
	 */
	public static HppResponse fromJsonHppResponse(String json) {
		try {
			return hppResponseReader.readValue(json);
		} catch (Exception ex) {
			LOGGER.error("Error creating HppResponse from JSON.", ex);
			throw new RealexException("Error creating HppResponse from JSON.", ex);
		}
	}

}
