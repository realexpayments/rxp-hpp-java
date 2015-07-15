package com.realexpayments.hpp.sdk;

/**
 * An exception class for general Realex SDK errors. All other SDK exceptions will extend this class.
 * 
 * @author markstanford
 */
public class RealexException extends RuntimeException {

	private static final long serialVersionUID = -2270549234447218179L;

	/**
	 * Constructor for RealexException.
	 * 
	 * @param message
	 * @param throwable
	 */
	public RealexException(String message, Throwable throwable) {
		super(message, throwable);
	}

	/**
	 * Constructor for RealexException.
	 * 
	 * @param message
	 */
	public RealexException(String message) {
		super(message);
	}

}
