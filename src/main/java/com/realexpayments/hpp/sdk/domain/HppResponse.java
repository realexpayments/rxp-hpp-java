package com.realexpayments.hpp.sdk.domain;

import java.io.UnsupportedEncodingException;
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.codec.binary.Base64;

import com.fasterxml.jackson.annotation.JsonAnyGetter;
import com.fasterxml.jackson.annotation.JsonAnySetter;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.realexpayments.hpp.sdk.utils.GenerationUtils;

/**
 * Class representing the HPP response.
 * 
 * @author markstanford
 *
 */
@JsonInclude(Include.NON_NULL)
public class HppResponse {

	/**
	 * This is the merchant id that Realex Payments assign to you.
	 */
	@JsonProperty("MERCHANT_ID")
	private String merchantId;

	/**
	 * The sub-account used in the transaction.
	 */
	@JsonProperty("ACCOUNT")
	private String account;

	/**
	 * The unique order id that you sent to us.
	 */
	@JsonProperty("ORDER_ID")
	private String orderId;

	/**
	 * The amount that was authorised. Returned in the lowest unit of the currency.
	 */
	@JsonProperty("AMOUNT")
	private String amount;

	/**
	 * Will contain a valid authcode if the transaction was successful. Will be empty otherwise.
	 */
	@JsonProperty("AUTHCODE")
	private String authCode;

	/**
	 * The date and time of the transaction.
	 */
	@JsonProperty("TIMESTAMP")
	private String timeStamp;

	/**
	 * A SHA-1 digital signature created using the HPP response fields and your shared secret.
	 */
	@JsonProperty("SHA1HASH")
	private String hash;

	/**
	 * The outcome of the transaction. Will contain "00" if the transaction was a success or another value (depending on the error) if not.
	 */
	@JsonProperty("RESULT")
	private String result;

	/**
	 * Will contain a text message that describes the result code.
	 */
	@JsonProperty("MESSAGE")
	private String message;

	/**
	 * <p>
	 * The result of the Card Verification check (if enabled):
	 * <ul>
	 * <li>M: CVV Matched.</li>
	 * <li>N: CVV Not Matched.</li>
	 * <li>I: CVV Not checked due to circumstances.</li>
	 * <li>U: CVV Not checked - issuer not certified.</li>
	 * <li>P: CVV Not Processed.</li>
	 * </ul>
	 * </p>
	 * 
	 */
	@JsonProperty("CVNRESULT")
	private String cvnResult;

	/**
	 * A unique reference that Realex Payments assign to your transaction.
	 */
	@JsonProperty("PASREF")
	private String pasRef;

	/**
	 * This is the Realex Payments batch that this transaction will be in. 
	 * (This is equal to "-1" if the transaction was sent in with the autosettle flag off. 
	 * After you settle it (either manually or programmatically) the response to that transaction will contain the batch id.)
	 */
	@JsonProperty("BATCHID")
	private String batchId;

	/**
	 * This is the ecommerce indicator (this will only be returned for 3DSecure transactions).
	 */
	@JsonProperty("ECI")
	private String eci;

	/**
	 * Cardholder Authentication Verification Value (this will only be returned for 3DSecure transactions).
	 */
	@JsonProperty("CAVV")
	private String cavv;

	/**
	 * Exchange Identifier (this will only be returned for 3DSecure transactions).
	 */
	@JsonProperty("XID")
	private String xid;

	/**
	 * Whatever data you have sent in the request will be returned to you.
	 */
	@JsonProperty("COMMENT1")
	private String commentOne;

	/**
	 * Whatever data you have sent in the request will be returned to you.
	 */
	@JsonProperty("COMMENT2")
	private String commentTwo;

	/**
	 * This field indicates whether the cardholder chose to save their card or not.
	 */@JsonProperty("REALWALLET_CHOSEN")
	private String realwalletChosen;

	/**
	 * Indicates whether or not the Payer (customer) reference was set up successfully.
	 */
	@JsonProperty("PAYER_SETUP")
	private String payerSetup;

	/**
	 * Result message text of the Payer setup attempt.
	 */
	@JsonProperty("PAYER_SETUP_MSG")
	private String payerSetupMsg;

	/**
	 * The Payer reference given to the customer.
	 */
	@JsonProperty("SAVED_PAYER_REF")
	private String savedPayerRef;

	/**
	 * Whether or not the customer's card was successfully stored.
	 */
	@JsonProperty("PMT_SETUP")
	private String pmtSetup;

	/**
	 * Result message text of the Payment Reference (card) setup attempt.
	 */
	@JsonProperty("PMT_SETUP_MSG")
	private String pmtSetupMsg;

	/**
	 * This is the card type of the&nbsp;Payment Reference stored.
	 */
	@JsonProperty("SAVED_PMT_TYPE")
	private String savedPmtType;

	/**
	 * The Payment Reference assigned to the card.
	 */
	@JsonProperty("SAVED_PMT_REF")
	private String savedPmtRef;

	/**
	 * The first six and last four digits of the card stored.
	 */
	@JsonProperty("SAVED_PMT_DIGITS")
	private String savedPmtDigits;

	/**
	 * This will contain the expiry date of the stored card for display purposes.
	 */
	@JsonProperty("SAVED_PMT_EXPDATE")
	private String savedPmtExpdate;

	/**
	 * The name of the person associated with the Payment Reference (the cardholder name).
	 */
	@JsonProperty("SAVED_PMT_NAME")
	private String savedPmtName;

	/**
	 * The Transaction Suitability Score for the transaction. The RealScore is comprised of various distinct tests. 
	 * Using the RealControl application you can request that Realex Payments return certain individual scores to you. 
	 * These are identified by numbers - thus TSS_1032 would be the result of the check with id 1032. 
	 * You can then use these specific checks in conjunction with RealScore score to ascertain whether or not you wish to continue with the settlement.
	 */
	@JsonProperty("TSS")
	@JsonDeserialize(as = HashMap.class)
	private Map<String, String> tss;

	/**
	 * Anything else you sent to us in the request will be returned to you in supplementary data.
	 */
	private Map<String, String> supplementaryData = new HashMap<String, String>();

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
	 * Getter for auth code. 
	 * 
	 * @return String
	 */
	public String getAuthCode() {
		return authCode;
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
	 * Getter for result.
	 * 
	 * @return String
	 */
	public String getResult() {
		return result;
	}

	/**
	 * Getter for message.
	 * 
	 * @return String
	 */
	public String getMessage() {
		return message;
	}

	/**
	 * Getter for CVN result.
	 * 
	 * @return String
	 */
	public String getCvnResult() {
		return cvnResult;
	}

	/**
	 * Getter for pas ref.
	 * 
	 * @return String
	 */
	public String getPasRef() {
		return pasRef;
	}

	/**
	 * Getter for batch ID.
	 * 
	 * @return String
	 */
	public String getBatchId() {
		return batchId;
	}

	/**
	 * Getter for ECI.
	 * 
	 * @return String
	 */
	public String getEci() {
		return eci;
	}

	/**
	 * Getter for CAAV.
	 * 
	 * @return String
	 */
	public String getCavv() {
		return cavv;
	}

	/**
	 * Getter for XID.
	 * 
	 * @return String
	 */
	public String getXid() {
		return xid;
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
	 * Getter for saved payer ref.
	 *
	 * @return String
	 */
	public String getSavedPayerRef() {
		return savedPayerRef;
	}

	/**
	 * Getter for realwallet chosen.
	 * @return String
	 */
	public String getRealwalletChosen() {
		return realwalletChosen;
	}

	/**
	 * Getter for payer setup.
	 * @return String
	 */
	public String getPayerSetup() {
		return payerSetup;
	}

	/**
	 * Getter for payer setup msg.
	 * @return String
	 */
	public String getPayerSetupMsg() {
		return payerSetupMsg;
	}

	/**
	 * Getter for pmt setup.
	 * @return String
	 */
	public String getPmtSetup() {
		return pmtSetup;
	}

	/**
	 * Getter for pmt setup msg.
	 * @return String
	 */
	public String getPmtSetupMsg() {
		return pmtSetupMsg;
	}

	/**
	 * Getter for saved pmt type.
	 * @return String
	 */
	public String getSavedPmtType() {
		return savedPmtType;
	}

	/**
	 * Getter for saved pmt ref.
	 * @return String
	 */
	public String getSavedPmtRef() {
		return savedPmtRef;
	}

	/**
	 * Getter for saved pmt digits.
	 * @return String
	 */
	public String getSavedPmtDigits() {
		return savedPmtDigits;
	}

	/**
	 * Getter for saved pmt expdate.
	 * @return String
	 */
	public String getSavedPmtExpdate() {
		return savedPmtExpdate;
	}

	/**
	 * Getter for saved pmt name.
	 * @return String
	 */
	public String getSavedPmtName() {
		return savedPmtName;
	}

	/**
	 * Getter for TSS map.
	 * 
	 * @return Map<String, String>
	 */
	public Map<String, String> getTss() {
		return tss;
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
	 * Setter for auth code. 
	 * 
	 * @param authCode
	 */
	public void setAuthCode(String authCode) {
		this.authCode = authCode;
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
	 * Setter for result.
	 * 
	 * @param result
	 */
	public void setResult(String result) {
		this.result = result;
	}

	/**
	 * Setter for message.
	 * 
	 * @param message
	 */
	public void setMessage(String message) {
		this.message = message;
	}

	/**
	 * Setter for CVN result.
	 * 
	 * @param cvnResult
	 */
	public void setCvnResult(String cvnResult) {
		this.cvnResult = cvnResult;
	}

	/**
	 * Setter for pas ref.
	 * 
	 * @param pasRef
	 */
	public void setPasRef(String pasRef) {
		this.pasRef = pasRef;
	}

	/**
	 * Setter for batch ID.
	 * 
	 * @param batchId
	 */
	public void setBatchId(String batchId) {
		this.batchId = batchId;
	}

	/**
	 * Setter for ECI.
	 * 
	 * @param eci
	 */
	public void setEci(String eci) {
		this.eci = eci;
	}

	/**
	 * Setter for CAVV.
	 * 
	 * @param cavv
	 */
	public void setCavv(String cavv) {
		this.cavv = cavv;
	}

	/**
	 * Setter for XID.
	 * 
	 * @param xid
	 */
	public void setXid(String xid) {
		this.xid = xid;
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
	 * Setter for TSS map.
	 * 
	 * @param tss
	 */
	public void setTss(Map<String, String> tss) {
		this.tss = tss;
	}

	/**
	 * Getter for supplementary data.
	 * 
	 * @return Map<String, String>
	 */
	@JsonAnyGetter
	public Map<String, String> getSupplementaryData() {
		return supplementaryData;
	}

	/**
	 * Setter for supplementary data value.
	 * 
	 * @param name
	 * @param value
	 */
	@JsonAnySetter
	public void setSupplementaryDataValue(String name, String value) {
		supplementaryData.put(name, value);
	}

	/**
	 * Creates the security hash from a number of fields and the shared secret. 
	 * 
	 * @param secret
	 * @return HppResponse
	 */
	public HppResponse hash(String secret) {
		this.hash = generateHash(secret);
		return this;
	}

	/**
	 * Creates the security hash from a number of fields and the shared secret. 
	 * 
	 * @param secret
	 * @return String
	 */
	private String generateHash(String secret) {

		//check for any null values and set them to empty string for hashing
		String timeStamp = null == this.timeStamp ? "" : this.timeStamp;
		String merchantId = null == this.merchantId ? "" : this.merchantId;
		String orderId = null == this.orderId ? "" : this.orderId;
		String result = null == this.result ? "" : this.result;
		String message = null == this.message ? "" : this.message;
		String pasRef = null == this.pasRef ? "" : this.pasRef;
		String authCode = null == this.authCode ? "" : this.authCode;

		//create String to hash
		String toHash = new StringBuilder().append(timeStamp)
				.append(".")
				.append(merchantId)
				.append(".")
				.append(orderId)
				.append(".")
				.append(result)
				.append(".")
				.append(message)
				.append(".")
				.append(pasRef)
				.append(".")
				.append(authCode)
				.toString();

		return GenerationUtils.generateHash(toHash, secret);
	}

	/**
	 * Base64 encodes the HPP response values.
	 * 
	 * @param charset
	 * @return HppResponse
	 * @throws UnsupportedEncodingException 
	 */
	public HppResponse encode(String charset) throws UnsupportedEncodingException {

		if (null != this.merchantId) {
			this.merchantId = new String(Base64.encodeBase64(this.merchantId.getBytes(charset)), charset);
		}
		if (null != this.account) {
			this.account = new String(Base64.encodeBase64(this.account.getBytes(charset)), charset);
		}
		if (null != this.amount) {
			this.amount = new String(Base64.encodeBase64(this.amount.getBytes(charset)), charset);
		}
		if (null != this.authCode) {
			this.authCode = new String(Base64.encodeBase64(this.authCode.getBytes(charset)), charset);
		}
		if (null != this.batchId) {
			this.batchId = new String(Base64.encodeBase64(this.batchId.getBytes(charset)), charset);
		}
		if (null != this.cavv) {
			this.cavv = new String(Base64.encodeBase64(this.cavv.getBytes(charset)), charset);
		}
		if (null != this.cvnResult) {
			this.cvnResult = new String(Base64.encodeBase64(this.cvnResult.getBytes(charset)), charset);
		}
		if (null != this.eci) {
			this.eci = new String(Base64.encodeBase64(this.eci.getBytes(charset)), charset);
		}
		if (null != this.commentOne) {
			this.commentOne = new String(Base64.encodeBase64(this.commentOne.getBytes(charset)), charset);
		}
		if (null != this.commentTwo) {
			this.commentTwo = new String(Base64.encodeBase64(this.commentTwo.getBytes(charset)), charset);
		}
		if (null != this.message) {
			this.message = new String(Base64.encodeBase64(this.message.getBytes(charset)), charset);
		}
		if (null != this.pasRef) {
			this.pasRef = new String(Base64.encodeBase64(this.pasRef.getBytes(charset)), charset);
		}
		if (null != this.hash) {
			this.hash = new String(Base64.encodeBase64(this.hash.getBytes(charset)), charset);
		}
		if (null != this.result) {
			this.result = new String(Base64.encodeBase64(this.result.getBytes(charset)), charset);
		}
		if (null != this.xid) {
			this.xid = new String(Base64.encodeBase64(this.xid.getBytes(charset)), charset);
		}
		if (null != this.orderId) {
			this.orderId = new String(Base64.encodeBase64(this.orderId.getBytes(charset)), charset);
		}
		if (null != this.timeStamp) {
			this.timeStamp = new String(Base64.encodeBase64(this.timeStamp.getBytes(charset)), charset);
		}
		if (null != this.realwalletChosen) {
			this.realwalletChosen = new String(Base64.encodeBase64(this.realwalletChosen.getBytes(charset)), charset);
		}
		if (null != this.payerSetup) {
			this.payerSetup = new String(Base64.encodeBase64(this.payerSetup.getBytes(charset)), charset);
		}
		if (null != this.payerSetupMsg) {
			this.payerSetupMsg = new String(Base64.encodeBase64(this.payerSetupMsg.getBytes(charset)), charset);
		}
		if (null != this.savedPayerRef) {
			this.savedPayerRef = new String(Base64.encodeBase64(this.savedPayerRef.getBytes(charset)), charset);
		}
		if (null != this.pmtSetup) {
			this.pmtSetup = new String(Base64.encodeBase64(this.pmtSetup.getBytes(charset)), charset);
		}
		if (null != this.pmtSetupMsg) {
			this.pmtSetupMsg = new String(Base64.encodeBase64(this.pmtSetupMsg.getBytes(charset)), charset);
		}
		if (null != this.savedPmtType) {
			this.savedPmtType = new String(Base64.encodeBase64(this.savedPmtType.getBytes(charset)), charset);
		}
		if (null != this.savedPmtRef) {
			this.savedPmtRef = new String(Base64.encodeBase64(this.savedPmtRef.getBytes(charset)), charset);
		}
		if (null != this.savedPmtDigits) {
			this.savedPmtDigits = new String(Base64.encodeBase64(this.savedPmtDigits.getBytes(charset)), charset);
		}
		if (null != this.savedPmtExpdate) {
			this.savedPmtExpdate = new String(Base64.encodeBase64(this.savedPmtExpdate.getBytes(charset)), charset);
		}
		if (null != this.savedPmtName) {
			this.savedPmtName = new String(Base64.encodeBase64(this.savedPmtName.getBytes(charset)), charset);
		}
		if (null != this.tss) {
			Map<String, String> tssMap = new HashMap<String, String>();
			for (String key : tss.keySet()) {
				tssMap.put(key, new String(Base64.encodeBase64(tss.get(key).getBytes(charset)), charset));
			}
			this.tss = new HashMap<String, String>();
			this.tss = tssMap;
		}
		if (null != this.supplementaryData) {
			Map<String, String> supplementaryDataMap = new HashMap<String, String>();
			for (String key : supplementaryData.keySet()) {
				supplementaryDataMap.put(key, new String(Base64.encodeBase64(supplementaryData.get(key).getBytes(charset)), charset));
			}
			this.supplementaryData = new HashMap<String, String>();
			this.supplementaryData.putAll(supplementaryDataMap);
		}
		return this;
	}

	/**
	 * Base64 decodes the HPP response values.
	 * 
	 * @param charset
	 * @return HppResponse
	 * @throws UnsupportedEncodingException 
	 */
	public HppResponse decode(String charset) throws UnsupportedEncodingException {

		if (null != this.merchantId) {
			this.merchantId = new String(Base64.decodeBase64(this.merchantId.getBytes(charset)), charset);
		}
		if (null != this.account) {
			this.account = new String(Base64.decodeBase64(this.account.getBytes(charset)), charset);
		}
		if (null != this.amount) {
			this.amount = new String(Base64.decodeBase64(this.amount.getBytes(charset)), charset);
		}
		if (null != this.authCode) {
			this.authCode = new String(Base64.decodeBase64(this.authCode.getBytes(charset)), charset);
		}
		if (null != this.batchId) {
			this.batchId = new String(Base64.decodeBase64(this.batchId.getBytes(charset)), charset);
		}
		if (null != this.cavv) {
			this.cavv = new String(Base64.decodeBase64(this.cavv.getBytes(charset)), charset);
		}
		if (null != this.cvnResult) {
			this.cvnResult = new String(Base64.decodeBase64(this.cvnResult.getBytes(charset)), charset);
		}
		if (null != this.eci) {
			this.eci = new String(Base64.decodeBase64(this.eci.getBytes(charset)), charset);
		}
		if (null != this.commentOne) {
			this.commentOne = new String(Base64.decodeBase64(this.commentOne.getBytes(charset)), charset);
		}
		if (null != this.commentTwo) {
			this.commentTwo = new String(Base64.decodeBase64(this.commentTwo.getBytes(charset)), charset);
		}
		if (null != this.message) {
			this.message = new String(Base64.decodeBase64(this.message.getBytes(charset)), charset);
		}
		if (null != this.pasRef) {
			this.pasRef = new String(Base64.decodeBase64(this.pasRef.getBytes(charset)), charset);
		}
		if (null != this.hash) {
			this.hash = new String(Base64.decodeBase64(this.hash.getBytes(charset)), charset);
		}
		if (null != this.result) {
			this.result = new String(Base64.decodeBase64(this.result.getBytes(charset)), charset);
		}
		if (null != this.xid) {
			this.xid = new String(Base64.decodeBase64(this.xid.getBytes(charset)), charset);
		}
		if (null != this.orderId) {
			this.orderId = new String(Base64.decodeBase64(this.orderId.getBytes(charset)), charset);
		}
		if (null != this.timeStamp) {
			this.timeStamp = new String(Base64.decodeBase64(this.timeStamp.getBytes(charset)), charset);
		}
		if (null != this.realwalletChosen) {
			this.realwalletChosen = new String(Base64.decodeBase64(this.realwalletChosen.getBytes(charset)), charset);
		}
		if (null != this.payerSetup) {
			this.payerSetup = new String(Base64.decodeBase64(this.payerSetup.getBytes(charset)), charset);
		}
		if (null != this.payerSetupMsg) {
			this.payerSetupMsg = new String(Base64.decodeBase64(this.payerSetupMsg.getBytes(charset)), charset);
		}
		if (null != this.savedPayerRef) {
			this.savedPayerRef = new String(Base64.decodeBase64(this.savedPayerRef.getBytes(charset)), charset);
		}
		if (null != this.pmtSetup) {
			this.pmtSetup = new String(Base64.decodeBase64(this.pmtSetup.getBytes(charset)), charset);
		}
		if (null != this.pmtSetupMsg) {
			this.pmtSetupMsg = new String(Base64.decodeBase64(this.pmtSetupMsg.getBytes(charset)), charset);
		}
		if (null != this.savedPmtType) {
			this.savedPmtType = new String(Base64.decodeBase64(this.savedPmtType.getBytes(charset)), charset);
		}
		if (null != this.savedPmtRef) {
			this.savedPmtRef = new String(Base64.decodeBase64(this.savedPmtRef.getBytes(charset)), charset);
		}
		if (null != this.savedPmtDigits) {
			this.savedPmtDigits = new String(Base64.decodeBase64(this.savedPmtDigits.getBytes(charset)), charset);
		}
		if (null != this.savedPmtExpdate) {
			this.savedPmtExpdate = new String(Base64.decodeBase64(this.savedPmtExpdate.getBytes(charset)), charset);
		}
		if (null != this.savedPmtName) {
			this.savedPmtName = new String(Base64.decodeBase64(this.savedPmtName.getBytes(charset)), charset);
		}
		if (null != this.tss) {
			Map<String, String> tssMap = new HashMap<String, String>();
			for (String key : tss.keySet()) {
				tssMap.put(key, new String(Base64.decodeBase64(tss.get(key).getBytes(charset)), charset));
			}
			this.tss = new HashMap<String, String>();
			this.tss = tssMap;
		}
		if (null != this.supplementaryData) {
			Map<String, String> supplementaryDataMap = new HashMap<String, String>();
			for (String key : supplementaryData.keySet()) {
				supplementaryDataMap.put(key, new String(Base64.decodeBase64(supplementaryData.get(key).getBytes(charset)), charset));
			}
			this.supplementaryData = new HashMap<String, String>();
			this.supplementaryData.putAll(supplementaryDataMap);
		}
		return this;
	}

	/**
	 * Helper method to determine if the HPP response security hash is valid. 
	 * 
	 * @param secret
	 * @return boolean 
	 */
	public boolean isHashValid(String secret) {
		String generatedHash = generateHash(secret);
		return generatedHash.equals(this.hash);
	}

}
