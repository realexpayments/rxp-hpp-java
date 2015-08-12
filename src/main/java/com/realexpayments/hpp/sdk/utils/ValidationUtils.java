package com.realexpayments.hpp.sdk.utils;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import javax.validation.ConstraintViolation;
import javax.validation.Validation;
import javax.validation.Validator;
import javax.validation.ValidatorFactory;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.realexpayments.hpp.sdk.RealexValidationException;
import com.realexpayments.hpp.sdk.domain.HppRequest;
import com.realexpayments.hpp.sdk.domain.HppResponse;

/**
 * Class validates HPP request and response objects.
 * 
 * @author markstanford
 */
public class ValidationUtils {

	/**
	 * Logger.
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(ValidationUtils.class);

	/**
	 * Validator object.
	 */
	private static Validator validator;

	/**
	 * Static initialiser instantiates validator object.
	 */
	static {
		ValidatorFactory factory = Validation.buildDefaultValidatorFactory();
		validator = factory.getValidator();
	}

	/**
	 * Method validates HPP request object using JSR-303 bean validation.
	 * 
	 * @param hppRequest
	 */
	public static void validate(HppRequest hppRequest) {
		Set<ConstraintViolation<HppRequest>> constraintViolations = validator.validate(hppRequest);

		if (constraintViolations.size() > 0) {
			List<String> validationMessages = new ArrayList<String>();
			Iterator<ConstraintViolation<HppRequest>> i = constraintViolations.iterator();

			while (i.hasNext()) {
				ConstraintViolation<HppRequest> constraitViolation = i.next();
				validationMessages.add(constraitViolation.getMessage());
			}

			LOGGER.info("HppRequest failed validation with the following errors {}", validationMessages);
			throw new RealexValidationException("HppRequest failed validation", validationMessages);
		}
	}

	/**
	 * Method validates HPP response hash.
	 * 
	 * @param hppResponse
	 * @param secret
	 */
	public static void validate(HppResponse hppResponse, String secret) {
		if (!hppResponse.isHashValid(secret)) {
			LOGGER.error("HppResponse contains an invalid security hash.");
			throw new RealexValidationException("HppResponse contains an invalid security hash");
		}
	}
}
