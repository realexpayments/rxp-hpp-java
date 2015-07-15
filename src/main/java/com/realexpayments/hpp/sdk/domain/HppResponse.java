package com.realexpayments.hpp.sdk.domain;

import java.util.HashMap;
import java.util.Map;

import org.apache.commons.codec.binary.Base64;

import com.fasterxml.jackson.annotation.JsonAnyGetter;
import com.fasterxml.jackson.annotation.JsonAnySetter;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.realexpayments.hpp.sdk.utils.GenerationUtils;

/**
 * @author markstanford
 *
 */
public class HppResponse {

	@JsonProperty("MERCHANT_ID")
	private String merchantId;

	@JsonProperty("ACCOUNT")
	private String account;

	@JsonProperty("ORDER_ID")
	private String orderId;

	@JsonProperty("AMOUNT")
	private String amount;

	@JsonProperty("AUTHCODE")
	private String authCode;

	@JsonProperty("TIMESTAMP")
	private String timeStamp;

	@JsonProperty("SHA1HASH")
	private String hash;

	@JsonProperty("RESULT")
	private String result;

	@JsonProperty("MESSAGE")
	private String message;

	@JsonProperty("CVNRESULT")
	private String cvnResult;

	@JsonProperty("PASREF")
	private String pasRef;

	@JsonProperty("BATCHID")
	private String batchId;

	@JsonProperty("ECI")
	private String eci;

	@JsonProperty("CAVV")
	private String cavv;

	@JsonProperty("XID")
	private String xid;

	@JsonProperty("COMMENT1")
	private String commentOne;

	@JsonProperty("COMMENT2")
	private String commentTwo;

	@JsonProperty("TSS")
	@JsonDeserialize(as = HashMap.class)
	private Map<String, String> tss;

	private Map<String, String> supplementaryData = new HashMap<String, String>();

	public String getMerchantId() {
		return merchantId;
	}

	public String getAccount() {
		return account;
	}

	public String getOrderId() {
		return orderId;
	}

	public String getAmount() {
		return amount;
	}

	public String getAuthCode() {
		return authCode;
	}

	public String getTimeStamp() {
		return timeStamp;
	}

	public String getHash() {
		return hash;
	}

	public String getResult() {
		return result;
	}

	public String getMessage() {
		return message;
	}

	public String getCvnResult() {
		return cvnResult;
	}

	public String getPasRef() {
		return pasRef;
	}

	public String getBatchId() {
		return batchId;
	}

	public String getEci() {
		return eci;
	}

	public String getCavv() {
		return cavv;
	}

	public String getXid() {
		return xid;
	}

	public String getCommentOne() {
		return commentOne;
	}

	public String getCommentTwo() {
		return commentTwo;
	}

	public Map<String, String> getTss() {
		return tss;
	}

	public void setMerchantId(String merchantId) {
		this.merchantId = merchantId;
	}

	public void setAccount(String account) {
		this.account = account;
	}

	public void setOrderId(String orderId) {
		this.orderId = orderId;
	}

	public void setAmount(String amount) {
		this.amount = amount;
	}

	public void setAuthCode(String authCode) {
		this.authCode = authCode;
	}

	public void setTimeStamp(String timeStamp) {
		this.timeStamp = timeStamp;
	}

	public void setHash(String hash) {
		this.hash = hash;
	}

	public void setResult(String result) {
		this.result = result;
	}

	public void setMessage(String message) {
		this.message = message;
	}

	public void setCvnResult(String cvnResult) {
		this.cvnResult = cvnResult;
	}

	public void setPasRef(String pasRef) {
		this.pasRef = pasRef;
	}

	public void setBatchId(String batchId) {
		this.batchId = batchId;
	}

	public void setEci(String eci) {
		this.eci = eci;
	}

	public void setCavv(String cavv) {
		this.cavv = cavv;
	}

	public void setXid(String xid) {
		this.xid = xid;
	}

	public void setCommentOne(String commentOne) {
		this.commentOne = commentOne;
	}

	public void setCommentTwo(String commentTwo) {
		this.commentTwo = commentTwo;
	}

	public void setTss(Map<String, String> tss) {
		this.tss = tss;
	}

	@JsonAnyGetter
	public Map<String, String> getSupplementaryData() {
		return supplementaryData;
	}

	@JsonAnySetter
	public void setUnknownField(String name, String value) {
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

	public HppResponse encode() {

		if (null != this.merchantId) {
			this.merchantId = new String(Base64.encodeBase64(this.merchantId.getBytes()));
		}
		if (null != this.account) {
			this.account = new String(Base64.encodeBase64(this.account.getBytes()));
		}
		if (null != this.amount) {
			this.amount = new String(Base64.encodeBase64(this.amount.getBytes()));
		}
		if (null != this.authCode) {
			this.authCode = new String(Base64.encodeBase64(this.authCode.getBytes()));
		}
		if (null != this.batchId) {
			this.batchId = new String(Base64.encodeBase64(this.batchId.getBytes()));
		}
		if (null != this.cavv) {
			this.cavv = new String(Base64.encodeBase64(this.cavv.getBytes()));
		}
		if (null != this.cvnResult) {
			this.cvnResult = new String(Base64.encodeBase64(this.cvnResult.getBytes()));
		}
		if (null != this.eci) {
			this.eci = new String(Base64.encodeBase64(this.eci.getBytes()));
		}
		if (null != this.commentOne) {
			this.commentOne = new String(Base64.encodeBase64(this.commentOne.getBytes()));
		}
		if (null != this.commentTwo) {
			this.commentTwo = new String(Base64.encodeBase64(this.commentTwo.getBytes()));
		}
		if (null != this.message) {
			this.message = new String(Base64.encodeBase64(this.message.getBytes()));
		}
		if (null != this.pasRef) {
			this.pasRef = new String(Base64.encodeBase64(this.pasRef.getBytes()));
		}
		if (null != this.hash) {
			this.hash = new String(Base64.encodeBase64(this.hash.getBytes()));
		}
		if (null != this.result) {
			this.result = new String(Base64.encodeBase64(this.result.getBytes()));
		}
		if (null != this.xid) {
			this.xid = new String(Base64.encodeBase64(this.xid.getBytes()));
		}
		if (null != this.orderId) {
			this.orderId = new String(Base64.encodeBase64(this.orderId.getBytes()));
		}
		if (null != this.timeStamp) {
			this.timeStamp = new String(Base64.encodeBase64(this.timeStamp.getBytes()));
		}
		if (null != this.tss) {
			Map<String, String> tssMap = new HashMap<String, String>();
			for (String key : tss.keySet()) {
				tssMap.put(key, new String(Base64.encodeBase64(tss.get(key).getBytes())));
			}
			this.tss = new HashMap<String, String>();
			this.tss = tssMap;
		}
		if (null != this.supplementaryData) {
			Map<String, String> supplementaryDataMap = new HashMap<String, String>();
			for (String key : supplementaryData.keySet()) {
				supplementaryDataMap.put(key, new String(Base64.encodeBase64(supplementaryData.get(key).getBytes())));
			}
			this.supplementaryData = new HashMap<String, String>();
			this.supplementaryData.putAll(supplementaryDataMap);
		}
		return this;
	}

	public HppResponse decode() {

		if (null != this.merchantId) {
			this.merchantId = new String(Base64.decodeBase64(this.merchantId.getBytes()));
		}
		if (null != this.account) {
			this.account = new String(Base64.decodeBase64(this.account.getBytes()));
		}
		if (null != this.amount) {
			this.amount = new String(Base64.decodeBase64(this.amount.getBytes()));
		}
		if (null != this.authCode) {
			this.authCode = new String(Base64.decodeBase64(this.authCode.getBytes()));
		}
		if (null != this.batchId) {
			this.batchId = new String(Base64.decodeBase64(this.batchId.getBytes()));
		}
		if (null != this.cavv) {
			this.cavv = new String(Base64.decodeBase64(this.cavv.getBytes()));
		}
		if (null != this.cvnResult) {
			this.cvnResult = new String(Base64.decodeBase64(this.cvnResult.getBytes()));
		}
		if (null != this.eci) {
			this.eci = new String(Base64.decodeBase64(this.eci.getBytes()));
		}
		if (null != this.commentOne) {
			this.commentOne = new String(Base64.decodeBase64(this.commentOne.getBytes()));
		}
		if (null != this.commentTwo) {
			this.commentTwo = new String(Base64.decodeBase64(this.commentTwo.getBytes()));
		}
		if (null != this.message) {
			this.message = new String(Base64.decodeBase64(this.message.getBytes()));
		}
		if (null != this.pasRef) {
			this.pasRef = new String(Base64.decodeBase64(this.pasRef.getBytes()));
		}
		if (null != this.hash) {
			this.hash = new String(Base64.decodeBase64(this.hash.getBytes()));
		}
		if (null != this.result) {
			this.result = new String(Base64.decodeBase64(this.result.getBytes()));
		}
		if (null != this.xid) {
			this.xid = new String(Base64.decodeBase64(this.xid.getBytes()));
		}
		if (null != this.orderId) {
			this.orderId = new String(Base64.decodeBase64(this.orderId.getBytes()));
		}
		if (null != this.timeStamp) {
			this.timeStamp = new String(Base64.decodeBase64(this.timeStamp.getBytes()));
		}
		if (null != this.tss) {
			Map<String, String> tssMap = new HashMap<String, String>();
			for (String key : tss.keySet()) {
				tssMap.put(key, new String(Base64.decodeBase64(tss.get(key).getBytes())));
			}
			this.tss = new HashMap<String, String>();
			this.tss = tssMap;
		}
		if (null != this.supplementaryData) {
			Map<String, String> supplementaryDataMap = new HashMap<String, String>();
			for (String key : supplementaryData.keySet()) {
				supplementaryDataMap.put(key, new String(Base64.decodeBase64(supplementaryData.get(key).getBytes())));
			}
			this.supplementaryData = new HashMap<String, String>();
			this.supplementaryData.putAll(supplementaryDataMap);
		}
		return this;
	}

	public boolean isHashValid(String secret) {
		String generatedHash = generateHash(secret);
		return generatedHash.equals(this.hash);
	}

}
