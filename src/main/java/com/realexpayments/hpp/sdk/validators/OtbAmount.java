package com.realexpayments.hpp.sdk.validators;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import javax.validation.Constraint;
import javax.validation.Payload;

/**
 * OTB amount validator. Amount must be set to 0 for OTB transactions. 
 * For OTB transactions the validate card only flag is set to 1.
 * 
 * @author markstanford
 *
 */
@Target({ ElementType.TYPE, ElementType.ANNOTATION_TYPE })
@Retention(RetentionPolicy.RUNTIME)
@Constraint(validatedBy = OtbAmountValidator.class)
@Documented
public @interface OtbAmount {

	String message() default "{hppRequest.amount.otb}";

	Class<?>[] groups() default {};

	Class<? extends Payload>[] payload() default {};
}
