package com.realexpayments.hpp.sdk.validators;

import javax.validation.ConstraintValidator;
import javax.validation.ConstraintValidatorContext;

import com.realexpayments.hpp.sdk.domain.HppRequest;

/**
 * OTB amount validator. Amount must be set to 0 for OTB transactions. 
 * For OTB transactions the validate card only flag is set to 1.
 * 
 * @author markstanford
 *
 */
public class OtbAmountValidator implements ConstraintValidator<OtbAmount, Object> {

	@Override
	public void initialize(OtbAmount constraintAnnotation) {
	}

	@Override
	public boolean isValid(Object value, ConstraintValidatorContext context) {
		boolean valid = true;

		HppRequest hppRequest = (HppRequest) value;

		//if validate card only flag is true (1), then ensure the amount is set to 0
		if (HppRequest.Flag.TRUE.getFlag().equals(hppRequest.getValidateCardOnly())) {
			if (!"0".equals(hppRequest.getAmount())) {
				valid = false;
			}
		}

		return valid;
	}

}
