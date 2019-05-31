package com.realexpayments.hpp.sdk.domain;

import java.io.UnsupportedEncodingException;
import java.util.HashMap;
import java.util.Map;

import javax.validation.constraints.Pattern;
import javax.validation.constraints.Size;

import org.apache.commons.codec.binary.Base64;

import com.fasterxml.jackson.annotation.JsonAnyGetter;
import com.fasterxml.jackson.annotation.JsonAnySetter;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.realexpayments.hpp.sdk.utils.GenerationUtils;
import com.realexpayments.hpp.sdk.validators.OtbAmount;

/**
 * <p>
 * Class representing a request to be sent to HPP.
 * </p>
 * <p>
 * Helper methods are provided (prefixed with 'add') for object creation.
 * </p>
 * <p>
 * Example usage:
 * <code><pre>
 * HppRequest request = new HppRequest()
 * 	.addAmount(100)
 * 	.addCurrency("EUR")
 * 	.addMerchantId("merchantId");
 * </pre></code>
 * </p>
 * 
 * @author markstanford
 */
@JsonInclude(Include.NON_NULL)
@OtbAmount
public class HppRequest {

	public enum Flag {
		TRUE("1"),
		FALSE("0");

		/**
		 * The flag String value 
		 */
		private final String flag;

		/**
		 * Flag constructor
		 * 
		 * @param flag
		 */
		Flag(String flag) {
			this.flag = flag;
		}

		/**
		 * Get the string value of the flag
		 * 
		 * @return String 
		 */
		public String getFlag() {
			return flag;
		}
	}

	/**
	 * The merchant ID supplied by Realex Payments – note this is not the merchant number supplied by your bank.
	 */
	@Size(min = 1, max = 50, message = "{hppRequest.merchantId.size}")
	@Pattern(regexp = "^[a-zA-Z0-9\\.]*$", message = "{hppRequest.merchantId.pattern}")
	@JsonProperty("MERCHANT_ID")
	private String merchantId;

	/**
	 * The sub-account to use for this transaction. If not present, the default sub-account will be used.
	 */
	@Size(min = 0, max = 30, message = "{hppRequest.account.size}")
	@Pattern(regexp = "^[a-zA-Z0-9\\s]*$", message = "{hppRequest.account.pattern}")
	@JsonProperty("ACCOUNT")
	private String account;

	/**
	 * A unique alphanumeric id that’s used to identify the transaction. No spaces are allowed.
	 */
	@Size(min = 0, max = 50, message = "{hppRequest.orderId.size}")
	@Pattern(regexp = "^[a-zA-Z0-9_\\-]*$*$", message = "{hppRequest.orderId.pattern}")
	@JsonProperty("ORDER_ID")
	private String orderId;

	/**
	 * Total amount to authorise in the lowest unit of the currency – i.e. 100 euro would be entered as 10000. 
	 * If there is no decimal in the currency (e.g. JPY Yen) then contact Realex Payments. No decimal points are allowed.
	 * Amount should be set to 0 for OTB transactions (i.e. where validate card only is set to 1).
	 */
	@Size(min = 1, max = 11, message = "{hppRequest.amount.size}")
	@Pattern(regexp = "^[0-9]*$", message = "{hppRequest.amount.pattern}")
	@JsonProperty("AMOUNT")
	private String amount;

	/**
	 * A three-letter currency code (Eg. EUR, GBP). A list of currency codes can be provided by your account manager.
	 */
	@Size(min = 3, max = 3, message = "{hppRequest.currency.size}")
	@Pattern(regexp = "^[a-zA-Z]*$", message = "{hppRequest.currency.pattern}")
	@JsonProperty("CURRENCY")
	private String currency;

	/**
	 * Date and time of the transaction. Entered in the following format: YYYYMMDDHHMMSS. Must be within 24 hours of the current time.
	 */
	@Size(min = 14, max = 14, message = "{hppRequest.timestamp.size}")
	@Pattern(regexp = "^[0-9]*$", message = "{hppRequest.timestamp.pattern}")
	@JsonProperty("TIMESTAMP")
	private String timeStamp;

	/**
	 * A digital signature generated using the SHA-1 algorithm.
	 */
	@Size(min = 40, max = 40, message = "{hppRequest.hash.size}")
	@Pattern(regexp = "^[a-f0-9]*$", message = "{hppRequest.hash.pattern}")
	@JsonProperty("SHA1HASH")
	private String hash;

	/**
	 * Used to signify whether or not you wish the transaction to be captured in the next batch. 
	 * If set to "1" and assuming the transaction is authorised then it will automatically be settled in the next batch. 
	 * If set to "0" then the merchant must use the RealControl application to manually settle the transaction. 
	 * This option can be used if a merchant wishes to delay the payment until after the goods have been shipped. 
	 * Transactions can be settled for up to 115% of the original amount and must be settled within a certain period of time agreed with your issuing bank.
	 */
	@Pattern(regexp = "(?i)^on*|^off$|^*$|^multi$|^1$|^0$", message = "{hppRequest.autoSettleFlag.pattern}")
	@JsonProperty("AUTO_SETTLE_FLAG")
	private String autoSettleFlag;

	/**
	 * A freeform comment to describe the transaction.
	 */
	@Size(min = 0, max = 255, message = "{hppRequest.comment1.size}")
	@Pattern(regexp = "^[\\s \u0020-\u003B \u003D \u003F-\u007E \u00A1-\u00FF\u20AC\u201A\u0192\u201E\u2026\u2020\u2021\u02C6\u2030\u0160\u2039\u0152\u017D\u2018\u2019\u201C\u201D\u2022\u2013\u2014\u02DC\u2122\u0161\u203A\u0153\u017E\u0178]*$", message = "{hppRequest.comment1.pattern}")
	@JsonProperty("COMMENT1")
	private String commentOne;

	/**
	 * A freeform comment to describe the transaction.
	 */
	@Size(min = 0, max = 255, message = "{hppRequest.comment2.size}")
	@Pattern(regexp = "^[\\s \u0020-\u003B \u003D \u003F-\u007E \u00A1-\u00FF\u20AC\u201A\u0192\u201E\u2026\u2020\u2021\u02C6\u2030\u0160\u2039\u0152\u017D\u2018\u2019\u201C\u201D\u2022\u2013\u2014\u02DC\u2122\u0161\u203A\u0153\u017E\u0178]*$", message = "{hppRequest.comment2.pattern}")
	@JsonProperty("COMMENT2")
	private String commentTwo;

	/**
	 * Used to signify whether or not you want a Transaction Suitability Score for this transaction. 
	 * Can be "0" for no and "1" for yes. 
	 */
	@Size(min = 0, max = 1, message = "{hppRequest.returnTss.size}")
	@Pattern(regexp = "^[01]*$", message = "{hppRequest.returnTss.pattern}")
	@JsonProperty("RETURN_TSS")
	private String returnTss;

	/**
	 * The postcode or ZIP of the shipping address.
	 */
	@Size(min = 0, max = 30, message = "{hppRequest.shippingCode.size}")
	@Pattern(regexp = "^[A-Za-z0-9\\,\\.\\-\\/\\| ]*$", message = "{hppRequest.shippingCode.pattern}")
	@JsonProperty("SHIPPING_CODE")
	private String shippingCode;

	/**
	 * The country of the shipping address.
	 */
	@Size(min = 0, max = 50, message = "{hppRequest.shippingCountry.size}")
	@Pattern(regexp = "^[A-Za-z0-9\\,\\.\\- ]*$", message = "{hppRequest.shippingCountry.pattern}")
	@JsonProperty("SHIPPING_CO")
	private String shippingCountry;

	/**
	 * The postcode or ZIP of the billing address.
	 */
	@Size(min = 0, max = 60, message = "{hppRequest.billingCode.size}")
	@Pattern(regexp = "^[A-Za-z0-9\\,\\.\\-\\/\\|\\* ]*$", message = "{hppRequest.billingCode.pattern}")
	@JsonProperty("BILLING_CODE")
	private String billingCode;

	/**
	 * The country of the billing address.
	 */
	@Size(min = 0, max = 50, message = "{hppRequest.billingCountry.size}")
	@Pattern(regexp = "^[A-Za-z0-9\\,\\.\\- ]*$", message = "{hppRequest.billingCountry.pattern}")
	@JsonProperty("BILLING_CO")
	private String billingCountry;

	/**
	 * The customer number of the customer. You can send in any additional information about the transaction in this field, 
	 * which will be visible under the transaction in the RealControl application.
	 */
	@Size(min = 0, max = 50, message = "{hppRequest.customerNumber.size}")
	@Pattern(regexp = "^[a-zA-Z0-9\\.\\_\\-\\,\\+\\@ \\s]*$", message = "{hppRequest.customerNumber.pattern}")
	@JsonProperty("CUST_NUM")
	private String customerNumber;

	/**
	 * A variable reference also associated with this customer. You can send in any additional information about the transaction in this field, 
	 * which will be visible under the transaction in the RealControl application.
	 */
	@Size(min = 0, max = 50, message = "{hppRequest.variableReference.size}")
	@Pattern(regexp = "^[a-zA-Z0-9\\.\\_\\-\\,\\+\\@ \\s]*$", message = "{hppRequest.variableReference.pattern}")
	@JsonProperty("VAR_REF")
	private String variableReference;

	/**
	 * A product id associated with this product. You can send in any additional information about the transaction in this field, 
	 * which will be visible under the transaction in the RealControl application.
	 */
	@Size(min = 0, max = 50, message = "{hppRequest.productId.size}")
	@Pattern(regexp = "^[a-zA-Z0-9\\.\\_\\-\\,\\+\\@ \\s]*$", message = "{hppRequest.productId.pattern}")
	@JsonProperty("PROD_ID")
	private String productId;

	/**
	 * Used to set what language HPP is displayed in. Currently HPP is available in English, Spanish and German, with other languages to follow. 
	 * If the field is not sent in, the default language is the language that is set in your account configuration. This can be set by your account manager.
	 */
	@Pattern(regexp = "^[a-zA-Z]{2}(_([a-zA-Z]{2}){1})?$|^$", message = "{hppRequest.language.pattern}")
	@JsonProperty("HPP_LANG")
	private String language;

	/**
	 * Used to set what text is displayed on the payment button for card transactions. If this field is not sent in, "Pay Now" is displayed on the button by default.
	 */
	@Size(min = 0, max = 25, message = "{hppRequest.cardPaymentButtonText.size}")
	@Pattern(regexp = "^[ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷ø¤ùúûüýþÿ\u0152\u017D\u0161\u0153\u017E\u0178¥a-zA-Z0-9\\'\\,\"\\+\\.\\_\\-\\&\\/\\@\\!\\?\\%\\()\\*\\:\\£\\$\\&\\u20AC\\#\\[\\]\\|\\=\\\\\u201C\u201D\u201C ]*$", message = "{hppRequest.cardPaymentButtonText.pattern}")
	@JsonProperty("CARD_PAYMENT_BUTTON")
	private String cardPaymentButtonText;

	/**
	 * Enable card storage.
	 */
	@Size(min = 0, max = 1, message = "{hppRequest.cardStorageEnable.size}")
	@Pattern(regexp = "^[10]*$", message = "{hppRequest.cardStorageEnable.pattern}")
	@JsonProperty("CARD_STORAGE_ENABLE")
	private String cardStorageEnable;

	/**
	 * Offer to save the card.
	 */
	@Size(min = 0, max = 1, message = "{hppRequest.offerSaveCard.size}")
	@Pattern(regexp = "^[01]*$", message = "{hppRequest.offerSaveCard.pattern}")
	@JsonProperty("OFFER_SAVE_CARD")
	private String offerSaveCard;

	/**
	 * The payer reference.
	 */
	@Size(min = 0, max = 50, message = "{hppRequest.payerReference.size}")
	@Pattern(regexp = "^[A-Za-z0-9\\_\\-\\\\ ]*$", message = "{hppRequest.payerReference.pattern}")
	@JsonProperty("PAYER_REF")
	private String payerReference;

	/**
	 * The payment reference.
	 */
	@Size(min = 0, max = 50, message = "{hppRequest.paymentReference.size}")
	@Pattern(regexp = "^[A-Za-z0-9\\_\\-]*$", message = "{hppRequest.paymentReference.pattern}")
	@JsonProperty("PMT_REF")
	private String paymentReference;

	/**
	 * Flag to indicate if the payer exists. 
	 */
	@Size(min = 0, max = 1, message = "{hppRequest.payerExists.size}")
	@Pattern(regexp = "^[102]*$", message = "{hppRequest.payerExists.pattern}")
	@JsonProperty("PAYER_EXIST")
	private String payerExists;

	/**
	 * Supplementary data to be sent to Realex Payments. This will be returned in the HPP response. 
	 */
	private Map<String, String> supplementaryData = new HashMap<String, String>();

	/**
	 * Used to identify an OTB transaction.
	 */
	@Size(min = 0, max = 1, message = "{hppRequest.validateCardOnly.size}")
	@Pattern(regexp = "^[01]*$", message = "{hppRequest.validateCardOnly.pattern}")
	@JsonProperty("VALIDATE_CARD_ONLY")
	private String validateCardOnly;

	/**
	 * Transaction level configuration to enable/disable a DCC request. (Only if the merchant is configured).
	 */
	@Size(min = 0, max = 1, message = "{hppRequest.dccEnable.size}")
	@Pattern(regexp = "^[01]*$", message = "{hppRequest.dccEnable.pattern}")
	@JsonProperty("DCC_ENABLE")
	private String dccEnable;

	/**
	 * Override merchant configuration for fraud. (Only if the merchant is configured for fraud).
	 */
	@Size(min = 0, max = 7, message = "{hppRequest.hppFraudFilterMode.size}")
	@Pattern(regexp = "^(ACTIVE|PASSIVE|OFF)*$", message = "{hppRequest.hppFraudFilterMode.pattern}")
	@JsonProperty("HPP_FRAUDFILTER_MODE")
	private String hppFraudFilterMode;

	/**
	 * The HPP Version. To use HPP Card Management select HPP_VERSION = 2.
	 */
	@Size(min = 0, max = 1, message = "{hppRequest.hppVersion.size}")
	@Pattern(regexp = "^[1-2]*$", message = "{hppRequest.hppVersion.pattern}")
	@JsonProperty("HPP_VERSION")
	@JsonInclude(Include.NON_EMPTY)
	private String hppVersion;

	/**
	 * The payer reference. If this flag is received, HPP will retrieve a list of the payment methods saved for that payer.
	 */
	@Size(min = 0, max = 50, message = "{hppRequest.hppSelectStoredCard.size}")
	@Pattern(regexp = "^[a-zA-Z0-9\\_\\-.\\s]*$", message = "{hppRequest.hppSelectStoredCard.pattern}")
	@JsonProperty("HPP_SELECT_STORED_CARD")
	@JsonInclude(Include.NON_EMPTY)
	private String hppSelectStoredCard;

    /**
     * Customer’s email address, including the full domain name. The field must be submitted in the form name@host.domain
     */
    @Size(max = 254, message = "{hppRequest.customerEmail.size}")
    @Pattern(regexp = "^([a-zA-Z0-9_\\-\\.]+)@([a-zA-Z0-9_\\-\\.]+)\\.([a-zA-Z]{2,24})*$", message = "{hppRequest.customerEmail.pattern}")
	@JsonProperty("HPP_CUSTOMER_EMAIL")
    private String customerEmail;

    /**
     *
     */
    @Size(max = 15, message = "{hppRequest.customerPhoneMobile.size}")
    @Pattern(regexp = "[0-9]*", message = "{hppRequest.customerPhoneMobile.pattern}")
    @JsonProperty("HPP_CUSTOMER_PHONENUMBER_MOBILE")
    private String customerPhoneMobile;

    /**
     * First line of the customer's billing address.
     */
    @Size(max = 50, message = "{hppRequest.billingAddress1.size}")
    @Pattern(regexp = "^[a-zA-Z0-9\\/\\-\\_\\.\\s\\(\\):;'',\"{}]*$", message = "{hppRequest.billingAddress1.pattern}")
    @JsonProperty("HPP_BILLING_STREET1")
    private String billingAddress1;

    /**
     * Second line of the customer's billing address. Can be submitted as
     * blank if not relevant for the particular customer.
     */
    @Size(max = 50, message = "{hppRequest.billingAddress2.size}")
    @Pattern(regexp = "^[a-zA-Z0-9\\/\\-\\_\\.\\s\\(\\):;'',\"{}]*$", message = "{hppRequest.billingAddress2.pattern}")
    @JsonProperty("HPP_BILLING_STREET2")
    private String billingAddress2;

    /**
     * Third line of the customer's billing address. Can be submitted as
     * blank if not relevant for the particular customer.
     */
    @Size(max = 50, message = "{hppRequest.billingAddress3.size}")
    @Pattern(regexp = "^[a-zA-Z0-9\\/\\-\\_\\.\\s\\(\\):;'',\"{}]*$", message = "{hppRequest.billingAddress3.pattern}")
    @JsonProperty("HPP_BILLING_STREET3")
    private String billingAddress3;

    /**
     * The city of the customer's billing address.
     */
    @Size(max = 50, message = "{hppRequest.billingAddressCity.size}")
    @Pattern(regexp = "^[a-zA-Z0-9\\/\\-\\_\\.\\s\\(\\):;'',\"{}]*$", message = "{hppRequest.billingAddressCity.pattern}")
    @JsonProperty("HPP_BILLING_CITY")
    private String billingAddressCity;

    /**
     * The state of the customer's billing address.
     * Should be the country subdivision code defined in ISO 3166-2.
     * Applicable for US and CA addresses.
     */
    @Size(max = 3, message = "{hppRequest.billingAddressState.size}")
    @Pattern(regexp = "^[a-zA-Z0-9\\/\\-\\_\\.\\s\\(\\):;'',\"{}]*$", message = "{hppRequest.billingAddressState.pattern}")
    @JsonProperty("HPP_BILLING_STATE")
    private String billingAddressState;

    /**
     * ZIP or other postal code customer's billing address.
     */
    @Size(max = 16, message = "{hppRequest.billingAddressPostalCode.size}")
    @Pattern(regexp = "^[a-zA-Z0-9-\\s]{1,16}$", message = "{hppRequest.billingAddressPostalCode.pattern}")
    @JsonProperty("HPP_BILLING_POSTALCODE")
    private String billingAddressPostalCode;

    /**
     * The country of the customer's billing address. ISO 3166-1 numeric three-digit country code.
     */
    @Size(max = 3, message = "{hppRequest.billingAddressCountry.size}")
    @Pattern(regexp = "^[0-9]{3}$", message = "{hppRequest.billingAddressCountry.pattern}")
    @JsonProperty("HPP_BILLING_COUNTRY")
    private String billingAddressCountry;

    /**
     * First line of the customer's shipping address.
     */
    @Size(max = 50, message = "{hppRequest.shippingAddress1.size}")
    @Pattern(regexp = "^[a-zA-Z0-9\\/\\-\\_\\.\\s\\(\\):;'',\"{}]*$", message = "{hppRequest.shippingAddress1.pattern}")
    @JsonProperty("HPP_SHIPPING_STREET1")
    private String shippingAddress1;

    /**
     * Second line of the customer's shipping address. Can be submitted as
     * blank if not relevant for the particular customer.
     */
    @Size(max = 50, message = "{hppRequest.shippingAddress2.size}")
    @Pattern(regexp = "^[a-zA-Z0-9\\/\\-\\_\\.\\s\\(\\):;'',\"{}]*$", message = "{hppRequest.shippingAddress2.pattern}")
    @JsonProperty("HPP_SHIPPING_STREET2")
    private String shippingAddress2;

    /**
     * Third line of the customer's shipping address. Can be submitted as
     * blank if not relevant for the particular customer.
     */
    @Size(max = 50, message = "{hppRequest.shippingAddress3.size}")
    @Pattern(regexp = "^[a-zA-Z0-9\\/\\-\\_\\.\\s\\(\\):;'',\"{}]*$", message = "{hppRequest.shippingAddress3.pattern}")
    @JsonProperty("HPP_SHIPPING_STREET3")
    private String shippingAddress3;

    /**
     * The city of the customer's shipping address.
     */
    @Size(max = 50, message = "{hppRequest.shippingAddressCity.size}")
    @Pattern(regexp = "^[a-zA-Z0-9\\/\\-\\_\\.\\s\\(\\):;'',\"{}]*$", message = "{hppRequest.shippingAddressCity.pattern}")
    @JsonProperty("HPP_SHIPPING_CITY")
    private String shippingAddressCity;

    /**
     * The state of the customer's shipping address. Should be the country subdivision code defined in ISO 3166-2.
     * Applicable for US and CA addresses.
     */
    @Size(max = 3, message = "{hppRequest.shippingAddressState.size}")
    @Pattern(regexp = "^[a-zA-Z0-9\\/\\-\\_\\.\\s\\(\\):;'',\"{}]*$", message = "{hppRequest.shippingAddressState.pattern}")
    @JsonProperty("HPP_SHIPPING_STATE")
    private String shippingAddressState;

    /**
     * ZIP or other postal code customer's shipping address.
     */
    @Size(max = 16, message = "{hppRequest.shippingAddressPostalCode.size}")
    @Pattern(regexp = "^[a-zA-Z0-9-\\s]{1,16}$", message = "{hppRequest.shippingAddressPostalCode.pattern}")
    @JsonProperty("HPP_SHIPPING_POSTALCODE")
    private String shippingAddressPostalCode;

    /**
     * The country of the customer's shipping address. ISO 3166-1 numeric three-digit country code.
     */
    @Size(max = 3, message = "{hppRequest.shippingAddressCountry.size}")
    @Pattern(regexp = "^[0-9]{3}$", message = "{hppRequest.shippingAddressCountry.pattern}")
    @JsonProperty("HPP_SHIPPING_COUNTRY")
    private String shippingAddressCountry;

    /**
     * Indicates whether the shipping address matches the billing address.
     */
    @Pattern(regexp = "^(TRUE|FALSE)*$", message = "{hppRequest.shippingAddressMatchIndicator.pattern}")
    @JsonProperty("HPP_ADDRESS_MATCH_INDICATOR")
    private String shippingAddressMatchIndicator;

    /**
     * Indicates whether a challenge is requested for this transaction.
     * The Issuer may override whatever preference is specified in this field.
     */
    @Pattern(regexp = "^(NO_PREFERENCE|NO_CHALLENGE_REQUESTED|CHALLENGE_PREFERRED|CHALLENGE_MANDATED)*$", message = "{hppRequest.challengeRequestIndicator.pattern}")
    @JsonProperty("HPP_CHALLENGE_REQUEST_INDICATOR")
    private String challengeRequestIndicator;

	/**
	 * Getter for merchant ID.
	 * 
	 * @return String
	 */
	public String getMerchantId() {
		return merchantId;
	}

	/**
	 * Getter for account.
	 * 
	 * @return String
	 */
	public String getAccount() {
		return account;
	}

	/**
	 * Getter for order ID.
	 * 
	 * @return String
	 */
	public String getOrderId() {
		return orderId;
	}

	/**
	 * Getter for amount.
	 * 
	 * @return String
	 */
	public String getAmount() {
		return amount;
	}

	/**
	 * Getter for currency.
	 * 
	 * @return String
	 */
	public String getCurrency() {
		return currency;
	}

	/**
	 * Getter for time stamp.
	 * 
	 * @return String
	 */
	public String getTimeStamp() {
		return timeStamp;
	}

	/**
	 * Getter for hash.
	 * 
	 * @return String
	 */
	public String getHash() {
		return hash;
	}

	/**
	 * Getter for auto settle flag.
	 * 
	 * @return String
	 */
	public String getAutoSettleFlag() {
		return autoSettleFlag;
	}

	/**
	 * Getter for comment one.
	 * 
	 * @return String
	 */
	public String getCommentOne() {
		return commentOne;
	}

	/**
	 * Getter for comment two.
	 * 
	 * @return String
	 */
	public String getCommentTwo() {
		return commentTwo;
	}

	/**
	 * Getter for return TSS flag.
	 * 
	 * @return String
	 */
	public String getReturnTss() {
		return returnTss;
	}

	/**
	 * Getter for shipping code. 
	 * 
	 * @return String
	 */
	public String getShippingCode() {
		return shippingCode;
	}

	/**
	 * Getter for shipping country.
	 * 
	 * @return String
	 */
	public String getShippingCountry() {
		return shippingCountry;
	}

	/**
	 * Getter for billing code. 
	 * 
	 * @return String
	 */
	public String getBillingCode() {
		return billingCode;
	}

	/**
	 * Getter for billing country.
	 * 
	 * @return String
	 */
	public String getBillingCountry() {
		return billingCountry;
	}

	/**
	 * Getter for customer number.
	 * 
	 * @return String
	 */
	public String getCustomerNumber() {
		return customerNumber;
	}

	/**
	 * Getter for variable reference. 
	 * 
	 * @return String
	 */
	public String getVariableReference() {
		return variableReference;
	}

	/**
	 * Getter for product ID.
	 * 
	 * @return String
	 */
	public String getProductId() {
		return productId;
	}

	/**
	 * Getter for language.
	 * 
	 * @return String
	 */
	public String getLanguage() {
		return language;
	}

	/**
	 * Getter for card payment button text.
	 * 
	 * @return String
	 */
	public String getCardPaymentButtonText() {
		return cardPaymentButtonText;
	}

	/**
	 * Getter for card storage enable flag.
	 * 
	 * @return String
	 */
	public String getCardStorageEnable() {
		return cardStorageEnable;
	}

	/**
	 * Getter for offer to save card.
	 * 
	 * @return String
	 */
	public String getOfferSaveCard() {
		return offerSaveCard;
	}

	/**
	 * Getter for payer reference.
	 * 
	 * @return String
	 */
	public String getPayerReference() {
		return payerReference;
	}

	/**
	 * Getter for payment reference.
	 * 
	 * @return String
	 */
	public String getPaymentReference() {
		return paymentReference;
	}

	/**
	 * Getter for payer exists. 
	 * 
	 * @return String
	 */
	public String getPayerExists() {
		return payerExists;
	}

	/**
	 * Getter for validate card only. 
	 * 
	 * @return String
	 */
	public String getValidateCardOnly() {
		return validateCardOnly;
	}

	/**
	 * Getter for DCC enable flag.
	 * 
	 * @return String
	 */
	public String getDccEnable() {
		return dccEnable;
	}

	/**
	 * Getter for HPP fraud filter mode flag.
	 * 
	 * @return String
	 */
	public String getHppFraudFilterMode() {
		return hppFraudFilterMode;
	}

    /**
     * Getter for customer email.
     *
     * @return String
     */
	public String getCustomerEmail() {
        return customerEmail;
    }

    /**
     * Getter for customer mobile phone
     *
     * @return String
     */
    public String getCustomerPhoneMobile() {
        return customerPhoneMobile;
    }

    /**
     * Getter for billing address 1
     *
     * @return String
     */
    public String getBillingAddress1() {
        return billingAddress1;
    }

    /**
     * Getter for billing address 2
     *
     * @return String
     */
    public String getBillingAddress2() {
        return billingAddress2;
    }

    /**
     * Getter for billing address 3
     *
     * @return String
     */
    public String getBillingAddress3() {
        return billingAddress3;
    }

    /**
     * Getter for billing address city
     *
     * @return String
     */
    public String getBillingAddressCity() {
        return billingAddressCity;
    }

    /**
     * Getter for billing address state
     *
     * @return String
     */
    public String getBillingAddressState() {
        return billingAddressState;
    }

    /**
     * Getter for billing address post code
     *
     * @return String
     */
    public String getBillingAddressPostalCode() {
        return billingAddressPostalCode;
    }

    /**
     * Getter for billing address country
     *
     * @return String
     */
    public String getBillingAddressCountry() {
        return billingAddressCountry;
    }

    /**
     * Getter for shipping address 1
     *
     * @return String
     */
    public String getShippingAddress1() {
        return shippingAddress1;
    }

    /**
     * Getter for shipping address 2
     *
     * @return String
     */
    public String getShippingAddress2() {
        return shippingAddress2;
    }

    /**
     * Getter for shipping address 3
     *
     * @return String
     */
    public String getShippingAddress3() {
        return shippingAddress3;
    }

    /**
     * Getter for shipping address city
     *
     * @return String
     */
    public String getShippingAddressCity() {
        return shippingAddressCity;
    }

    /**
     * Getter for shipping address state
     *
     * @return String
     */
    public String getShippingAddressState() {
        return shippingAddressState;
    }

    /**
     * Getter for shipping address post code
     *
     * @return String
     */
    public String getShippingAddressPostalCode() {
        return shippingAddressPostalCode;
    }

    /**
     * Getter for shipping address country
     *
     * @return String
     */
    public String getShippingAddressCountry() {
        return shippingAddressCountry;
    }

    /**
     * Getter for shipping address match indicator
     *
     * @return String
     */
    public String getShippingAddressMatchIndicator() {
        return shippingAddressMatchIndicator;
    }

    /**
     * Getter for challenge request indicator
     *
     * @return String
     */
    public String getChallengeRequestIndicator() {
        return challengeRequestIndicator;
    }

    /**
	 * Setter for merchant ID.
	 * 
	 * @param merchantId
	 */
	public void setMerchantId(String merchantId) {
		this.merchantId = merchantId;
	}

	/**
	 * Setter for account. 
	 * 
	 * @param account
	 */
	public void setAccount(String account) {
		this.account = account;
	}

	/**
	 * Setter for order ID.
	 * 
	 * @param orderId
	 */
	public void setOrderId(String orderId) {
		this.orderId = orderId;
	}

	/**
	 * Setter for amount. 
	 * 
	 * @param amount
	 */
	public void setAmount(String amount) {
		this.amount = amount;
	}

	/**
	 * Setter for currency.
	 * 
	 * @param currency
	 */
	public void setCurrency(String currency) {
		this.currency = currency;
	}

	/**
	 * Setter for time stamp.
	 * 
	 * @param timeStamp
	 */
	public void setTimeStamp(String timeStamp) {
		this.timeStamp = timeStamp;
	}

	/**
	 * Setter for hash.
	 * 
	 * @param hash
	 */
	public void setHash(String hash) {
		this.hash = hash;
	}

	/**
	 * Setter for auto settle flag.
	 * 
	 * @param autoSettleFlag
	 */
	public void setAutoSettleFlag(String autoSettleFlag) {
		this.autoSettleFlag = autoSettleFlag;
	}

	/**
	 * Setter for comment one. 
	 * 
	 * @param commentOne
	 */
	public void setCommentOne(String commentOne) {
		this.commentOne = commentOne;
	}

	/**
	 * Setter for comment two. 
	 * 
	 * @param commentTwo
	 */
	public void setCommentTwo(String commentTwo) {
		this.commentTwo = commentTwo;
	}

	/**
	 * Setter for return TSS.
	 * 
	 * @param returnTss
	 */
	public void setReturnTss(String returnTss) {
		this.returnTss = returnTss;
	}

	/**
	 * Setter for shipping code. 
	 * 
	 * @param shippingCode
	 */
	public void setShippingCode(String shippingCode) {
		this.shippingCode = shippingCode;
	}

	/**
	 * Setter for shipping country.
	 * 
	 * @param shippingCountry
	 */
	public void setShippingCountry(String shippingCountry) {
		this.shippingCountry = shippingCountry;
	}

	/**
	 * Setter for billing code. 
	 * 
	 * @param billingCode
	 */
	public void setBillingCode(String billingCode) {
		this.billingCode = billingCode;
	}

	/**
	 * Setter for billing country.
	 * 
	 * @param billingCountry
	 */
	public void setBillingCountry(String billingCountry) {
		this.billingCountry = billingCountry;
	}

	/**
	 * Setter for customer number. 
	 * 
	 * @param customerNumber
	 */
	public void setCustomerNumber(String customerNumber) {
		this.customerNumber = customerNumber;
	}

	/**
	 * Setter for variable reference. 
	 * 
	 * @param variableReference
	 */
	public void setVariableReference(String variableReference) {
		this.variableReference = variableReference;
	}

	/**
	 * Setter for product ID.
	 * 
	 * @param productId
	 */
	public void setProductId(String productId) {
		this.productId = productId;
	}

	/**
	 * Setter for language. 
	 * 
	 * @param language
	 */
	public void setLanguage(String language) {
		this.language = language;
	}

	/**
	 * Setter for card payment button text. 
	 * 
	 * @param cardPaymentButtonText
	 */
	public void setCardPaymentButtonText(String cardPaymentButtonText) {
		this.cardPaymentButtonText = cardPaymentButtonText;
	}

	/**
	 * Setter for card storage enable flag. 
	 * 
	 * @param cardStorageEnable
	 */
	public void setCardStorageEnable(String cardStorageEnable) {
		this.cardStorageEnable = cardStorageEnable;
	}

	/**
	 * Setter for offer to save card. 
	 * 
	 * @param offerSaveCard
	 */
	public void setOfferSaveCard(String offerSaveCard) {
		this.offerSaveCard = offerSaveCard;
	}

	/**
	 * Setter for payer reference. 
	 * 
	 * @param payerReference
	 */
	public void setPayerReference(String payerReference) {
		this.payerReference = payerReference;
	}

	/**
	 * Setter for payment reference.
	 * 
	 * @param paymentReference
	 */
	public void setPaymentReference(String paymentReference) {
		this.paymentReference = paymentReference;
	}

	/**
	 * Setter for payer exists. 
	 * 
	 * @param payerExists
	 */
	public void setPayerExists(String payerExists) {
		this.payerExists = payerExists;
	}

	/**
	 * Setter for validate card only.
	 * 
	 * @param validateCardOnly
	 */
	public void setValidateCardOnly(String validateCardOnly) {
		this.validateCardOnly = validateCardOnly;
	}

	/**
	 * Setter for DCC enable flag.
	 * 
	 * @param dccEnable
	 */
	public void setDccEnable(String dccEnable) {
		this.dccEnable = dccEnable;
	}

	/**
	 * Setter for HPP fraud filter mode flag.
	 * 
	 * @param hppFraudFilterMode
	 */
	public void setHppFraudFilterMode(String hppFraudFilterMode) {
		this.hppFraudFilterMode = hppFraudFilterMode;
	}

    /**
     * Setter for customer email.
     *
     * @param customerEmail
     */
    public void setCustomerEmail(String customerEmail) {
        this.customerEmail = customerEmail;
    }

    /**
     * Setter for customer mobile phone.
     *
     * @param customerPhoneMobile
     */
    public void setCustomerPhoneMobile(String customerPhoneMobile) {
        this.customerPhoneMobile = customerPhoneMobile;
    }

    /**
     * Setter for billing address 1.
     *
     * @param billingAddress1
     */
    public void setBillingAddress1(String billingAddress1) {
        this.billingAddress1 = billingAddress1;
    }

    /**
     * Setter for billing address 2.
     *
     * @param billingAddress2
     */
    public void setBillingAddress2(String billingAddress2) {
        this.billingAddress2 = billingAddress2;
    }

    /**
     * Setter for billing address 3.
     *
     * @param billingAddress3
     */
    public void setBillingAddress3(String billingAddress3) {
        this.billingAddress3 = billingAddress3;
    }

    /**
     * Setter for billing address city.
     *
     * @param billingAddressCity
     */
    public void setBillingAddressCity(String billingAddressCity) {
        this.billingAddressCity = billingAddressCity;
    }

    /**
     * Setter for billing address state.
     *
     * @param billingAddressState
     */
    public void setBillingAddressState(String billingAddressState) {
        this.billingAddressState = billingAddressState;
    }

    /**
     * Setter for billing address post code
     *
     * @param billingAddressPostalCode
     */
    public void setBillingAddressPostalCode(String billingAddressPostalCode) {
        this.billingAddressPostalCode = billingAddressPostalCode;
    }

    /**
     * Setter for billing address country
     *
     * @param billingAddressCountry
     */
    public void setBillingAddressCountry(String billingAddressCountry) {
        this.billingAddressCountry = billingAddressCountry;
    }

    /**
     * Setter for shipping address 1
     *
     * @param shippingAddress1
     */
    public void setShippingAddress1(String shippingAddress1) {
        this.shippingAddress1 = shippingAddress1;
    }

    /**
     * Setter for shipping address 2
     *
     * @param shippingAddress2
     */
    public void setShippingAddress2(String shippingAddress2) {
        this.shippingAddress2 = shippingAddress2;
    }

    /**
     * Setter for shipping address 3
     *
     * @param shippingAddress3
     */
    public void setShippingAddress3(String shippingAddress3) {
        this.shippingAddress3 = shippingAddress3;
    }

    /**
     * Setter for shipping address city
     *
     * @param shippingAddressCity
     */
    public void setShippingAddressCity(String shippingAddressCity) {
        this.shippingAddressCity = shippingAddressCity;
    }

    /**
     * Setter for shipping address state
     *
     * @param shippingAddressState
     */
    public void setShippingAddressState(String shippingAddressState) {
        this.shippingAddressState = shippingAddressState;
    }

    /**
     * Setter for shipping address post code
     *
     * @param shippingAddressPostalCode
     */
    public void setShippingAddressPostalCode(String shippingAddressPostalCode) {
        this.shippingAddressPostalCode = shippingAddressPostalCode;
    }

    /**
     * Setter for shipping address country
     *
     * @param shippingAddressCountry
     */
    public void setShippingAddressCountry(String shippingAddressCountry) {
        this.shippingAddressCountry = shippingAddressCountry;
    }

    /**
     * Setter for shipping address match indicator
     *
     * @param shippingAddressMatchIndicator
     */
    public void setShippingAddressMatchIndicator(String shippingAddressMatchIndicator) {
        this.shippingAddressMatchIndicator = shippingAddressMatchIndicator;
    }

    /**
     * Setter for challenge request indicator
     *
     * @param challengeRequestIndicator
     */
    public void setChallengeRequestIndicator(String challengeRequestIndicator) {
        this.challengeRequestIndicator = challengeRequestIndicator;
    }

    /**
	 * Helper method to add merchant ID.
	 * 
	 * @param merchantId
	 * @return HppRequest
	 */
	public HppRequest addMerchantId(String merchantId) {
		this.merchantId = merchantId;
		return this;
	}

	/**
	 * Helper method to add account. 
	 * 
	 * @param account
	 * @return HppRequest
	 */
	public HppRequest addAccount(String account) {
		this.account = account;
		return this;
	}

	/**
	 * Helper method to add order ID.
	 * 
	 * @param orderId
	 * @return HppRequest
	 */
	public HppRequest addOrderId(String orderId) {
		this.orderId = orderId;
		return this;
	}

	/**
	 * Helper method to add amount.
	 * 
	 * @param amount
	 * @return HppRequest
	 */
	public HppRequest addAmount(long amount) {
		this.amount = String.valueOf(amount);
		return this;
	}

	/**
	 * Helper method to add amount.
	 * 
	 * @param amount
	 * @return HppRequest
	 */
	public HppRequest addAmount(String amount) {
		this.amount = amount;
		return this;
	}

	/**
	 * Helper method to add currency.
	 * 
	 * @param currency
	 * @return HppRequest
	 */
	public HppRequest addCurrency(String currency) {
		this.currency = currency;
		return this;
	}

	/**
	 * Helper method to add time stamp.
	 * 
	 * @param timeStamp
	 * @return HppRequest
	 */
	public HppRequest addTimeStamp(String timeStamp) {
		this.timeStamp = timeStamp;
		return this;
	}

	/**
	 * Helper method to add hash.
	 * 
	 * @param hash
	 * @return HppRequest
	 */
	public HppRequest addHash(String hash) {
		this.hash = hash;
		return this;
	}

	/**
	 * Helper method to add autop settle flag.
	 * 
	 * @param autoSettleFlag
	 * @return HppRequest
	 */
	public HppRequest addAutoSettleFlag(boolean autoSettleFlag) {
		this.autoSettleFlag = autoSettleFlag ? Flag.TRUE.getFlag() : Flag.FALSE.getFlag();
		return this;
	}

	/**
	 * Helper method to add auto settle flag.
	 * 
	 * @param autoSettleFlag
	 * @return HppRequest
	 */
	public HppRequest addAutoSettleFlag(String autoSettleFlag) {
		this.autoSettleFlag = autoSettleFlag;
		return this;
	}

	/**
	 * Helper method to add comment one. 
	 * 
	 * @param commentOne
	 * @return HppRequest
	 */
	public HppRequest addCommentOne(String commentOne) {
		this.commentOne = commentOne;
		return this;
	}

	/**
	 * Helper method to add comment two.
	 * 
	 * @param commentTwo
	 * @return HppRequest
	 */
	public HppRequest addCommentTwo(String commentTwo) {
		this.commentTwo = commentTwo;
		return this;
	}

	/**
	 * Helper method to add return TSS.
	 * 
	 * @param returnTss
	 * @return HppRequest
	 */
	public HppRequest addReturnTss(boolean returnTss) {
		this.returnTss = returnTss ? Flag.TRUE.getFlag() : Flag.FALSE.getFlag();
		return this;
	}

	/**
	 * Helper method to add return TSS.
	 * 
	 * @param returnTss
	 * @return HppRequest
	 */
	public HppRequest addReturnTss(String returnTss) {
		this.returnTss = returnTss;
		return this;
	}

	/**
	 * Helper method to add shipping code. 
	 * 
	 * @param shippingCode
	 * @return HppRequest
	 */
	public HppRequest addShippingCode(String shippingCode) {
		this.shippingCode = shippingCode;
		return this;
	}

	/**
	 * Helper method to add hipping country.
	 * 
	 * @param shippingCountry
	 * @return HppRequest
	 */
	public HppRequest addShippingCountry(String shippingCountry) {
		this.shippingCountry = shippingCountry;
		return this;
	}

	/**
	 * Helper method to add billing code. 
	 * 
	 * @param billingCode
	 * @return HppRequest
	 */
	public HppRequest addBillingCode(String billingCode) {
		this.billingCode = billingCode;
		return this;
	}

	/**
	 * Helper method to add billing country.
	 * 
	 * @param billingCountry
	 * @return HppRequest
	 */
	public HppRequest addBillingCountry(String billingCountry) {
		this.billingCountry = billingCountry;
		return this;
	}

	/**
	 * Helper method to add customer number. 
	 * 
	 * @param customerNumber
	 * @return HppRequest
	 */
	public HppRequest addCustomerNumber(String customerNumber) {
		this.customerNumber = customerNumber;
		return this;
	}

	/**
	 * Helper method to add variable reference. 
	 * 
	 * @param variableReference
	 * @return HppRequest
	 */
	public HppRequest addVariableReference(String variableReference) {
		this.variableReference = variableReference;
		return this;
	}

	/**
	 * Helper method to add product ID.
	 * 
	 * @param productId
	 * @return HppRequest
	 */
	public HppRequest addProductId(String productId) {
		this.productId = productId;
		return this;
	}

	/**
	 * Helper method to add language.
	 * 
	 * @param language
	 * @return HppRequest
	 */
	public HppRequest addLanguage(String language) {
		this.language = language;
		return this;
	}

	/**
	 * Helper method to add card payment button text.
	 * 
	 * @param cardPaymentButtonText
	 * @return HppRequest
	 */
	public HppRequest addCardPaymentButtonText(String cardPaymentButtonText) {
		this.cardPaymentButtonText = cardPaymentButtonText;
		return this;
	}

	/**
	 * Helper method to add card storage enable flag.
	 * 
	 * @param cardStorageEnable
	 * @return HppRequest
	 */
	public HppRequest addCardStorageEnable(boolean cardStorageEnable) {
		this.cardStorageEnable = cardStorageEnable ? Flag.TRUE.getFlag() : Flag.FALSE.getFlag();
		return this;
	}

	/**
	 * Helper method to add card storage enable flag.
	 * 
	 * @param cardStorageEnable
	 * @return HppRequest
	 */
	public HppRequest addCardStorageEnable(String cardStorageEnable) {
		this.cardStorageEnable = cardStorageEnable;
		return this;
	}

	/**
	 * Helper method to add offer to save card.
	 * 
	 * @param offerSaveCard
	 * @return HppRequest
	 */
	public HppRequest addOfferSaveCard(boolean offerSaveCard) {
		this.offerSaveCard = offerSaveCard ? Flag.TRUE.getFlag() : Flag.FALSE.getFlag();
		return this;
	}

	/**
	 * Helper method to add offer to save card.
	 * 
	 * @param offerSaveCard
	 * @return HppRequest
	 */
	public HppRequest addOfferSaveCard(String offerSaveCard) {
		this.offerSaveCard = offerSaveCard;
		return this;
	}

	/**
	 * Helper method to add payer reference.
	 * 
	 * @param payerReference
	 * @return HppRequest
	 */
	public HppRequest addPayerReference(String payerReference) {
		this.payerReference = payerReference;
		return this;
	}

	/**
	 * Helper method to add payment reference.
	 * 
	 * @param paymentReference
	 * @return HppRequest
	 */
	public HppRequest addPaymentReference(String paymentReference) {
		this.paymentReference = paymentReference;
		return this;
	}

	/**
	 * Helper method to add payer exists flag.
	 * 
	 * @param payerExists
	 * @return HppRequest
	 */
	public HppRequest addPayerExists(boolean payerExists) {
		this.payerExists = payerExists ? Flag.TRUE.getFlag() : Flag.FALSE.getFlag();
		return this;
	}

	/**
	 * Helper method to add payer exists flag.
	 * 
	 * @param payerExists
	 * @return HppRequest
	 */
	public HppRequest addPayerExists(String payerExists) {
		this.payerExists = payerExists;
		return this;
	}

	/**
	 * Helper method to get supplementary data.
	 * 
	 * @return Map<String, String>
	 */
	@JsonAnyGetter
	public Map<String, String> getSupplementaryData() {
		return supplementaryData;
	}

	/**
	 * Setter for supplementary data.
	 * 
	 * @param supplementaryData
	 */
	public void setSupplementaryData(Map<String, String> supplementaryData) {
		this.supplementaryData = supplementaryData;
	}

	/**
	 * Helper method to add supplementary data.
	 * 
	 * @param name
	 * @param value
	 * @return HppRequest
	 */
	@JsonAnySetter
	public HppRequest addSupplementaryDataValue(String name, String value) {
		supplementaryData.put(name, value);
		return this;
	}

	/**
	 * Helper method to add validate card only flag.
	 * 
	 * @param validateCardOnly
	 * @return HppRequest
	 */
	public HppRequest addValidateCardOnly(boolean validateCardOnly) {
		this.validateCardOnly = validateCardOnly ? Flag.TRUE.getFlag() : Flag.FALSE.getFlag();
		return this;
	}

	/**
	 * Helper method to add validate card only flag.
	 * 
	 * @param validateCardOnly
	 * @return HppRequest
	 */
	public HppRequest addValidateCardOnly(String validateCardOnly) {
		this.validateCardOnly = validateCardOnly;
		return this;
	}

	/**
	 * Helper method to add DCC enable flag.
	 * 
	 * @param dccEnable
	 * @return HppRequest
	 */
	public HppRequest addDccEnable(boolean dccEnable) {
		this.dccEnable = dccEnable ? Flag.TRUE.getFlag() : Flag.FALSE.getFlag();
		return this;
	}

	/**
	 * Helper method to add DCC enable flag.
	 * 
	 * @param dccEnable
	 * @return HppRequest
	 */
	public HppRequest addDccEnable(String dccEnable) {
		this.dccEnable = dccEnable;
		return this;
	}

	/**
	 * Helper method to add HPP fraud filter mode flag.
	 * 
	 * @param hppFraudFilterMode
	 * @return HppRequest
	 */
	public HppRequest addHppFraudFilterMode(String hppFraudFilterMode) {
		this.hppFraudFilterMode = hppFraudFilterMode;
		return this;
	}

	/**
	 * Helper method to add HPP Version flag.
	 * 
	 * @param hppVersion
	 * @return HppRequest
	 */
	public HppRequest addHppVersion(String hppVersion) {
		this.hppVersion = hppVersion;
		return this;
	}

	/**
	 * Helper method to add HPP Select stored card.
	 * 
	 * @param hppSelectStoredCard
	 * @return HppRequest
	 */
	public HppRequest addHppSelectStoredCard(String hppSelectStoredCard) {
		this.hppSelectStoredCard = hppSelectStoredCard;
		return this;
	}

    /**
     * Helper method to add customer email.
     *
     * @param customerEmail
     * @return HppRequest
     */
    public HppRequest addCustomerEmail (String customerEmail) {
        this.customerEmail = customerEmail;
        return this;
    }

    /**
     * Helper method to add customer mobile number.
     *
     * @param customerPhoneMobile
     * @return HppRequest
     */
    public HppRequest addCustomerPhoneMobile (String customerPhoneMobile) {
        this.customerPhoneMobile = customerPhoneMobile;
        return this;
    }

    /**
     * Helper method to add billing address 1.
     *
     * @param billingAddress1
     * @return HppRequest
     */
    public HppRequest addBillingAddress1 (String billingAddress1) {
        this.billingAddress1 = billingAddress1;
        return this;
    }

    /**
     * Helper method to add billing address 2.
     *
     * @param billingAddress2
     * @return HppRequest
     */
    public HppRequest addBillingAddress2 (String billingAddress2) {
        this.billingAddress2 = billingAddress2;
        return this;
    }

    /**
     * Helper method to add billing address 3.
     *
     * @param billingAddress3
     * @return HppRequest
     */
    public HppRequest addBillingAddress3 (String billingAddress3) {
        this.billingAddress3 = billingAddress3;
        return this;
    }

    /**
     * Helper method to add billing address city.
     *
     * @param billingAddressCity
     * @return HppRequest
     */
    public HppRequest addBillingAddressCity (String billingAddressCity) {
        this.billingAddressCity = billingAddressCity;
        return this;
    }

    /**
     * Helper method to add billing address state.
     *
     * @param billingAddressState
     * @return HppRequest
     */
    public HppRequest addBillingAddressState (String billingAddressState) {
        this.billingAddressState = billingAddressState;
        return this;
    }

    /**
     * Helper method to add billing address postal code.
     *
     * @param billingAddressPostalCode
     * @return HppRequest
     */
    public HppRequest addBillingAddressPostalCode (String billingAddressPostalCode) {
        this.billingAddressPostalCode = billingAddressPostalCode;
        return this;
    }

    /**
     * Helper method to add .
     *
     * @param billingAddressCountry
     * @return HppRequest
     */
    public HppRequest addBillingAddressCountry (String billingAddressCountry) {
        this.billingAddressCountry = billingAddressCountry;
        return this;
    }

    /**
     * Helper method to add shipping address 1.
     *
     * @param shippingAddress1
     * @return HppRequest
     */
    public HppRequest addShippingAddress1 (String shippingAddress1) {
        this.shippingAddress1 = shippingAddress1;
        return this;
    }

    /**
     * Helper method to add shipping address 2.
     *
     * @param shippingAddress2
     * @return HppRequest
     */
    public HppRequest addShippingAddress2 (String shippingAddress2) {
        this.shippingAddress2 = shippingAddress2;
        return this;
    }

    /**
     * Helper method to add shipping address 3.
     *
     * @param shippingAddress3
     * @return HppRequest
     */
    public HppRequest addShippingAddress3 (String shippingAddress3) {
        this.shippingAddress3 = shippingAddress3;
        return this;
    }

    /**
     * Helper method to add shipping address city.
     *
     * @param shippingAddressCity
     * @return HppRequest
     */
    public HppRequest addsShippingAddressCity (String shippingAddressCity) {
        this.shippingAddressCity = shippingAddressCity;
        return this;
    }

    /**
     * Helper method to add shipping address state.
     *
     * @param shippingAddressState
     * @return HppRequest
     */
    public HppRequest addShippingAddressState (String shippingAddressState) {
        this.shippingAddressState = shippingAddressState;
        return this;
    }

    /**
     * Helper method to add shipping address postal code.
     *
     * @param shippingAddressPostalCode
     * @return HppRequest
     */
    public HppRequest addShippingAddressPostalCode (String shippingAddressPostalCode) {
        this.shippingAddressPostalCode = shippingAddressPostalCode;
        return this;
    }

    /**
     * Helper method to add shipping address country.
     *
     * @param shippingAddressCountry
     * @return HppRequest
     */
    public HppRequest addShippingAddressCountry (String shippingAddressCountry) {
        this.shippingAddressCountry = shippingAddressCountry;
        return this;
    }

    /**
     * Helper method to add shipping address match indicator.
     *
     * @param shippingAddressMatchIndicator
     * @return HppRequest
     */
    public HppRequest addShippingAddressMatchIndicator (String shippingAddressMatchIndicator) {
        this.shippingAddressMatchIndicator = shippingAddressMatchIndicator;
        return this;
    }

    /**
     * Helper method to add challenge request indicator.
     *
     * @param challengeRequestIndicator
     * @return HppRequest
     */
    public HppRequest addChallengeRequestIndicator (String challengeRequestIndicator) {
        this.challengeRequestIndicator = challengeRequestIndicator;
        return this;
    }

	/**
	 * Get hppSelectStoredCard
	 * 
	 * @return String hppSelectStoredCard
	 */
	public String getHppSelectStoredCard() {
		return hppSelectStoredCard;
	}

	/**
	 * Set hppSelectStoredCard
	 * @param hppSelectStoredCard
	 */
	public void setHppSelectStoredCard(String hppSelectStoredCard) {
		this.hppSelectStoredCard = hppSelectStoredCard;
	}

	/**
	 * Creates the security hash from a number of fields and the shared secret. 
	 * 
	 * @param secret
	 * @return HppRequest
	 */
	public HppRequest hash(String secret) {

		// Override payerRef with hppSelectStoredCard if present.
		if (this.hppSelectStoredCard != null && !"".equalsIgnoreCase(this.hppSelectStoredCard)) {
			this.payerReference = this.hppSelectStoredCard;
		}

		//check for any null values and set them to empty string for hashing
		String timeStamp = null == this.timeStamp ? "" : this.timeStamp;
		String merchantId = null == this.merchantId ? "" : this.merchantId;
		String orderId = null == this.orderId ? "" : this.orderId;
		String amount = null == this.amount ? "" : this.amount;
		String currency = null == this.currency ? "" : this.currency;
		String payerReference = null == this.payerReference ? "" : this.payerReference;
		String paymentReference = null == this.paymentReference ? "" : this.paymentReference;
		String hppFraudFilterMode = null == this.hppFraudFilterMode ? "" : this.hppFraudFilterMode;

		//create String to hash. Check for card storage enable flag to determine if Real Vault transaction 
		StringBuilder toHash = new StringBuilder();

		if (Flag.TRUE.getFlag().equals(cardStorageEnable) || (hppSelectStoredCard != null && !hppSelectStoredCard.isEmpty())) {
			toHash.append(timeStamp).append(".").append(merchantId).append(".").append(orderId).append(".").append(amount).append(".")
					.append(currency).append(".").append(payerReference).append(".").append(paymentReference);

			if (!hppFraudFilterMode.equals("")) {
				toHash.append(".").append(this.hppFraudFilterMode);
			}

		} else {
			toHash.append(timeStamp).append(".").append(merchantId).append(".").append(orderId).append(".").append(amount).append(".")
					.append(currency);

			if (!hppFraudFilterMode.equals("")) {
				toHash.append(".").append(this.hppFraudFilterMode);
			}
		}

		this.hash = GenerationUtils.generateHash(toHash.toString(), secret);

		return this;
	}

	/**
	 * Generates default values for fields such as hash, timestamp and order ID.
	 * 
	 * @param secret
	 * @return HppRequest
	 */
	public HppRequest generateDefaults(String secret) {

		//generate timestamp if not set
		if (null == this.timeStamp || "".equals(this.timeStamp)) {
			this.timeStamp = GenerationUtils.generateTimestamp();
		}

		//generate order ID if not set
		if (null == this.orderId || "".equals(this.orderId)) {
			this.orderId = GenerationUtils.generateOrderId();
		}

		//generate hash
		hash(secret);

		return this;
	}

	/**
	 * Base64 encodes all Hpp Request values.
	 * 
	 * @param charset
	 * @return HppRequest
	 * @throws UnsupportedEncodingException 
	 */
	public HppRequest encode(String charset) throws UnsupportedEncodingException {

		if (null != this.account) {
			this.account = new String(Base64.encodeBase64(this.account.getBytes(charset)), charset);
		}
		if (null != this.amount) {
			this.amount = new String(Base64.encodeBase64(this.amount.getBytes(charset)), charset);
		}
		if (null != this.autoSettleFlag) {
			this.autoSettleFlag = new String(Base64.encodeBase64(this.autoSettleFlag.getBytes(charset)), charset);
		}
		if (null != this.billingCode) {
			this.billingCode = new String(Base64.encodeBase64(this.billingCode.getBytes(charset)), charset);
		}
		if (null != this.billingCountry) {
			this.billingCountry = new String(Base64.encodeBase64(this.billingCountry.getBytes(charset)), charset);
		}
		if (null != this.cardPaymentButtonText) {
			this.cardPaymentButtonText = new String(Base64.encodeBase64(this.cardPaymentButtonText.getBytes(charset)), charset);
		}
		if (null != this.cardStorageEnable) {
			this.cardStorageEnable = new String(Base64.encodeBase64(this.cardStorageEnable.getBytes(charset)), charset);
		}
		if (null != this.commentOne) {
			this.commentOne = new String(Base64.encodeBase64(this.commentOne.getBytes(charset)), charset);
		}
		if (null != this.commentTwo) {
			this.commentTwo = new String(Base64.encodeBase64(this.commentTwo.getBytes(charset)), charset);
		}
		if (null != this.currency) {
			this.currency = new String(Base64.encodeBase64(this.currency.getBytes(charset)), charset);
		}
		if (null != this.customerNumber) {
			this.customerNumber = new String(Base64.encodeBase64(this.customerNumber.getBytes(charset)), charset);
		}
		if (null != this.hash) {
			this.hash = new String(Base64.encodeBase64(this.hash.getBytes(charset)), charset);
		}
		if (null != this.language) {
			this.language = new String(Base64.encodeBase64(this.language.getBytes(charset)), charset);
		}
		if (null != this.merchantId) {
			this.merchantId = new String(Base64.encodeBase64(this.merchantId.getBytes(charset)), charset);
		}
		if (null != this.offerSaveCard) {
			this.offerSaveCard = new String(Base64.encodeBase64(this.offerSaveCard.getBytes(charset)), charset);
		}
		if (null != this.orderId) {
			this.orderId = new String(Base64.encodeBase64(this.orderId.getBytes(charset)), charset);
		}
		if (null != this.payerExists) {
			this.payerExists = new String(Base64.encodeBase64(this.payerExists.getBytes(charset)), charset);
		}
		if (null != this.payerReference) {
			this.payerReference = new String(Base64.encodeBase64(this.payerReference.getBytes(charset)), charset);
		}
		if (null != this.paymentReference) {
			this.paymentReference = new String(Base64.encodeBase64(this.paymentReference.getBytes(charset)), charset);
		}
		if (null != this.productId) {
			this.productId = new String(Base64.encodeBase64(this.productId.getBytes(charset)), charset);
		}
		if (null != this.returnTss) {
			this.returnTss = new String(Base64.encodeBase64(this.returnTss.getBytes(charset)), charset);
		}
		if (null != this.shippingCode) {
			this.shippingCode = new String(Base64.encodeBase64(this.shippingCode.getBytes(charset)), charset);
		}
		if (null != this.shippingCountry) {
			this.shippingCountry = new String(Base64.encodeBase64(this.shippingCountry.getBytes(charset)), charset);
		}
		if (null != this.timeStamp) {
			this.timeStamp = new String(Base64.encodeBase64(this.timeStamp.getBytes(charset)), charset);
		}
		if (null != this.variableReference) {
			this.variableReference = new String(Base64.encodeBase64(this.variableReference.getBytes(charset)));
		}

		if (null != this.supplementaryData) {
			Map<String, String> supplementaryDataMap = new HashMap<String, String>();
			for (String key : supplementaryData.keySet()) {
				supplementaryDataMap.put(key, new String(Base64.encodeBase64(supplementaryData.get(key).getBytes(charset)), charset));
			}
			this.supplementaryData.putAll(supplementaryDataMap);
		}
		if (null != this.validateCardOnly) {
			this.validateCardOnly = new String(Base64.encodeBase64(this.validateCardOnly.getBytes(charset)));
		}
		if (null != this.dccEnable) {
			this.dccEnable = new String(Base64.encodeBase64(this.dccEnable.getBytes(charset)));
		}
		if (null != this.hppFraudFilterMode) {
			this.hppFraudFilterMode = new String(Base64.encodeBase64(this.hppFraudFilterMode.getBytes(charset)));
		}
		if (null != this.hppVersion) {
			this.hppVersion = new String(Base64.encodeBase64(this.hppVersion.getBytes(charset)));
		}
		if (null != this.hppSelectStoredCard) {
			this.hppSelectStoredCard = new String(Base64.encodeBase64(this.hppSelectStoredCard.getBytes(charset)));
		}

		// 3DS2 FIELDS
        if(null != this.customerEmail) {
            this.customerEmail = new String(Base64.encodeBase64(this.customerEmail.getBytes(charset)));
        }
        if(null != this.customerPhoneMobile) {
            this.customerPhoneMobile = new String(Base64.encodeBase64(this.customerPhoneMobile.getBytes(charset)));
        }
        if(null != this.billingAddress1) {
            this.billingAddress1 = new String(Base64.encodeBase64(this.billingAddress1.getBytes(charset)));
        }
        if(null != this.billingAddress2) {
            this.billingAddress2 = new String(Base64.encodeBase64(this.billingAddress2.getBytes(charset)));
        }
        if(null != this.billingAddress3) {
            this.billingAddress3 = new String(Base64.encodeBase64(this.billingAddress3.getBytes(charset)));
        }
        if(null != this.billingAddressCity) {
            this.billingAddressCity = new String(Base64.encodeBase64(this.billingAddressCity.getBytes(charset)));
        }
        if(null != this.billingAddressState) {
            this.billingAddressState = new String(Base64.encodeBase64(this.billingAddressState.getBytes(charset)));
        }
        if(null != this.billingAddressPostalCode) {
            this.billingAddressPostalCode = new String(Base64.encodeBase64(this.billingAddressPostalCode.getBytes(charset)));
        }
        if(null != this.billingAddressCountry) {
            this.billingAddressCountry = new String(Base64.encodeBase64(this.billingAddressCountry.getBytes(charset)));
        }
        if(null != this.shippingAddress1) {
            this.shippingAddress1 = new String(Base64.encodeBase64(this.shippingAddress1.getBytes(charset)));
        }
        if(null != this.shippingAddress2) {
            this.shippingAddress2 = new String(Base64.encodeBase64(this.shippingAddress2.getBytes(charset)));
        }
        if(null != this.shippingAddress3) {
            this.shippingAddress3 = new String(Base64.encodeBase64(this.shippingAddress3.getBytes(charset)));
        }
        if(null != this.shippingAddressCity) {
            this.shippingAddressCity = new String(Base64.encodeBase64(this.shippingAddressCity.getBytes(charset)));
        }
        if(null != this.shippingAddressState) {
            this.shippingAddressState = new String(Base64.encodeBase64(this.shippingAddressState.getBytes(charset)));
        }
        if(null != this.shippingAddressPostalCode) {
            this.shippingAddressPostalCode = new String(Base64.encodeBase64(this.shippingAddressPostalCode.getBytes(charset)));
        }
        if(null != this.shippingAddressCountry) {
            this.shippingAddressCountry = new String(Base64.encodeBase64(this.shippingAddressCountry.getBytes(charset)));
        }
        if(null != this.shippingAddressMatchIndicator) {
            this.shippingAddressMatchIndicator = new String(Base64.encodeBase64(this.shippingAddressMatchIndicator.getBytes(charset)));
        }
        if(null != this.challengeRequestIndicator) {
            this.challengeRequestIndicator = new String(Base64.encodeBase64(this.challengeRequestIndicator.getBytes(charset)));
        }

		return this;
	}

	/**
	 * Base64 decodes all Hpp Request values.
	 * 
	 * @param charset
	 * @return HppRequest
	 * @throws UnsupportedEncodingException 
	 */
	public HppRequest decode(String charset) throws UnsupportedEncodingException {

		if (null != this.account) {
			this.account = new String(Base64.decodeBase64(this.account.getBytes(charset)), charset);
		}
		if (null != this.amount) {
			this.amount = new String(Base64.decodeBase64(this.amount.getBytes(charset)), charset);
		}
		if (null != this.autoSettleFlag) {
			this.autoSettleFlag = new String(Base64.decodeBase64(this.autoSettleFlag.getBytes(charset)), charset);
		}
		if (null != this.billingCode) {
			this.billingCode = new String(Base64.decodeBase64(this.billingCode.getBytes(charset)), charset);
		}
		if (null != this.billingCountry) {
			this.billingCountry = new String(Base64.decodeBase64(this.billingCountry.getBytes(charset)), charset);
		}
		if (null != this.cardPaymentButtonText) {
			this.cardPaymentButtonText = new String(Base64.decodeBase64(this.cardPaymentButtonText.getBytes(charset)), charset);
		}
		if (null != this.cardStorageEnable) {
			this.cardStorageEnable = new String(Base64.decodeBase64(this.cardStorageEnable.getBytes(charset)), charset);
		}
		if (null != this.commentOne) {
			this.commentOne = new String(Base64.decodeBase64(this.commentOne.getBytes(charset)), charset);
		}
		if (null != this.commentTwo) {
			this.commentTwo = new String(Base64.decodeBase64(this.commentTwo.getBytes(charset)), charset);
		}
		if (null != this.currency) {
			this.currency = new String(Base64.decodeBase64(this.currency.getBytes(charset)), charset);
		}
		if (null != this.customerNumber) {
			this.customerNumber = new String(Base64.decodeBase64(this.customerNumber.getBytes(charset)), charset);
		}
		if (null != this.hash) {
			this.hash = new String(Base64.decodeBase64(this.hash.getBytes(charset)), charset);
		}
		if (null != this.language) {
			this.language = new String(Base64.decodeBase64(this.language.getBytes(charset)), charset);
		}
		if (null != this.merchantId) {
			this.merchantId = new String(Base64.decodeBase64(this.merchantId.getBytes(charset)), charset);
		}
		if (null != this.offerSaveCard) {
			this.offerSaveCard = new String(Base64.decodeBase64(this.offerSaveCard.getBytes(charset)), charset);
		}
		if (null != this.orderId) {
			this.orderId = new String(Base64.decodeBase64(this.orderId.getBytes(charset)), charset);
		}
		if (null != this.payerExists) {
			this.payerExists = new String(Base64.decodeBase64(this.payerExists.getBytes(charset)), charset);
		}
		if (null != this.payerReference) {
			this.payerReference = new String(Base64.decodeBase64(this.payerReference.getBytes(charset)), charset);
		}
		if (null != this.paymentReference) {
			this.paymentReference = new String(Base64.decodeBase64(this.paymentReference.getBytes(charset)), charset);
		}
		if (null != this.productId) {
			this.productId = new String(Base64.decodeBase64(this.productId.getBytes(charset)), charset);
		}
		if (null != this.returnTss) {
			this.returnTss = new String(Base64.decodeBase64(this.returnTss.getBytes(charset)), charset);
		}
		if (null != this.shippingCode) {
			this.shippingCode = new String(Base64.decodeBase64(this.shippingCode.getBytes(charset)), charset);
		}
		if (null != this.shippingCountry) {
			this.shippingCountry = new String(Base64.decodeBase64(this.shippingCountry.getBytes(charset)), charset);
		}
		if (null != this.timeStamp) {
			this.timeStamp = new String(Base64.decodeBase64(this.timeStamp.getBytes(charset)), charset);
		}
		if (null != this.variableReference) {
			this.variableReference = new String(Base64.decodeBase64(this.variableReference.getBytes(charset)));
		}

		if (null != this.supplementaryData) {
			Map<String, String> supplementaryDataMap = new HashMap<String, String>();
			for (String key : supplementaryData.keySet()) {
				supplementaryDataMap.put(key, new String(Base64.decodeBase64(supplementaryData.get(key).getBytes(charset)), charset));
			}
			this.supplementaryData.putAll(supplementaryDataMap);
		}
		if (null != this.validateCardOnly) {
			this.validateCardOnly = new String(Base64.decodeBase64(this.validateCardOnly.getBytes(charset)));
		}
		if (null != this.dccEnable) {
			this.dccEnable = new String(Base64.decodeBase64(this.dccEnable.getBytes(charset)));
		}
		if (null != this.hppFraudFilterMode) {
			this.hppFraudFilterMode = new String(Base64.decodeBase64(this.hppFraudFilterMode.getBytes(charset)));
		}
		if (null != this.hppVersion) {
			this.hppVersion = new String(Base64.decodeBase64(this.hppVersion.getBytes(charset)));
		}
		if (null != this.hppSelectStoredCard) {
			this.hppSelectStoredCard = new String(Base64.decodeBase64(this.hppSelectStoredCard.getBytes(charset)));
		}

		// 3DS2 FIELDS
        if(null != this.customerEmail) {
            this.customerEmail = new String(Base64.decodeBase64(this.customerEmail.getBytes(charset)));
        }

        if(null != this.customerPhoneMobile) {
            this.customerPhoneMobile = new String(Base64.decodeBase64(this.customerPhoneMobile.getBytes(charset)));
        }

        if(null != this.billingAddress1) {
            this.billingAddress1 = new String(Base64.decodeBase64(this.billingAddress1.getBytes(charset)));
        }

        if(null != this.billingAddress2) {
            this.billingAddress2 = new String(Base64.decodeBase64(this.billingAddress2.getBytes(charset)));
        }

        if(null != this.billingAddress3) {
            this.billingAddress3 = new String(Base64.decodeBase64(this.billingAddress3.getBytes(charset)));
        }

        if(null != this.billingAddressCity) {
            this.billingAddressCity = new String(Base64.decodeBase64(this.billingAddressCity.getBytes(charset)));
        }

        if(null != this.billingAddressState) {
            this.billingAddressState = new String(Base64.decodeBase64(this.billingAddressState.getBytes(charset)));
        }

        if(null != this.billingAddressPostalCode) {
            this.billingAddressPostalCode = new String(Base64.decodeBase64(this.billingAddressPostalCode.getBytes(charset)));
        }

        if(null != this.billingAddressCountry) {
            this.billingAddressCountry = new String(Base64.decodeBase64(this.billingAddressCountry.getBytes(charset)));
        }

        if(null != this.shippingAddress1) {
            this.shippingAddress1 = new String(Base64.decodeBase64(this.shippingAddress1.getBytes(charset)));
        }

        if(null != this.shippingAddress2) {
            this.shippingAddress2 = new String(Base64.decodeBase64(this.shippingAddress2.getBytes(charset)));
        }

        if(null != this.shippingAddress3) {
            this.shippingAddress3 = new String(Base64.decodeBase64(this.shippingAddress3.getBytes(charset)));
        }

        if(null != this.shippingAddressCity) {
            this.shippingAddressCity = new String(Base64.decodeBase64(this.shippingAddressCity.getBytes(charset)));
        }

        if(null != this.shippingAddressState) {
            this.shippingAddressState = new String(Base64.decodeBase64(this.shippingAddressState.getBytes(charset)));
        }

        if(null != this.shippingAddressPostalCode) {
            this.shippingAddressPostalCode = new String(Base64.decodeBase64(this.shippingAddressPostalCode.getBytes(charset)));
        }

        if(null != this.shippingAddressCountry) {
            this.shippingAddressCountry = new String(Base64.decodeBase64(this.shippingAddressCountry.getBytes(charset)));
        }

        if(null != this.shippingAddressMatchIndicator) {
            this.shippingAddressMatchIndicator = new String(Base64.decodeBase64(this.shippingAddressMatchIndicator.getBytes(charset)));
        }

        if(null != this.challengeRequestIndicator) {
            this.challengeRequestIndicator = new String(Base64.decodeBase64(this.challengeRequestIndicator.getBytes(charset)));
        }

		return this;
	}

}
