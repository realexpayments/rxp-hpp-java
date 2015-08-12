package com.realexpayments.hpp.sdk;

import java.util.List;

/**
 * An exception class for general Realex SDK validation errors.
 * 
 * @author markstanford
 *
 */
public class RealexValidationException extends RealexException {

	private static final long serialVersionUID = -5328243688578577594L;

	/**
	 * List of validation messages.
	 */
	private List<String> validationMessages;

	/**
	 * Getter for validation messages list.
	 * 
	 * @return List<String>
	 */
	public List<String> getValidationMessages() {
		return validationMessages;
	}

	/**
	 * Constructor for RealexValidationException.
	 * 
	 * @param message
	 */
	public RealexValidationException(String message) {
		super(message);
	}

	/**
	 * Constructor for RealexValidationException.
	 * 
	 * @param message
	 * @param validationMessages List of validation failure messages
	 */
	public RealexValidationException(String message, List<String> validationMessages) {
		super(message);
		this.validationMessages = validationMessages;
	}

}
