;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Indent-tabs-mode: NIL -*-
;;;
;;; grovel.lisp --- Foreign constants and structs.
;;;
;;; Copyright (C) 2008, Stelian Ionescu  <sionescu@common-lisp.net>
;;;
;;; This code is free software; you can redistribute it and/or
;;; modify it under the terms of the version 2.1 of
;;; the GNU Lesser General Public License as published by
;;; the Free Software Foundation, as clarified by the
;;; preamble found here:
;;;     http://opensource.franz.com/preamble.html
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General
;;; Public License along with this library; if not, write to the
;;; Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
;;; Boston, MA 02110-1301, USA

(include "gcrypt.h" "gnutls/gnutls.h" "gnutls/extra.h")

(in-package :net.tls)

(constantenum gnutls-error-codes
  ((:gnutls-e-unknown-compression-algorithm "GNUTLS_E_UNKNOWN_COMPRESSION_ALGORITHM"))
  ((:gnutls-e-unknown-cipher-type "GNUTLS_E_UNKNOWN_CIPHER_TYPE"))
  ((:gnutls-e-large-packet "GNUTLS_E_LARGE_PACKET"))
  ((:gnutls-e-unsupported-version-packet "GNUTLS_E_UNSUPPORTED_VERSION_PACKET"))
  ((:gnutls-e-unexpected-packet-length "GNUTLS_E_UNEXPECTED_PACKET_LENGTH"))
  ((:gnutls-e-invalid-session "GNUTLS_E_INVALID_SESSION"))
  ((:gnutls-e-fatal-alert-received "GNUTLS_E_FATAL_ALERT_RECEIVED"))
  ((:gnutls-e-unexpected-packet "GNUTLS_E_UNEXPECTED_PACKET"))
  ((:gnutls-e-warning-alert-received "GNUTLS_E_WARNING_ALERT_RECEIVED"))
  ((:gnutls-e-error-in-finished-packet "GNUTLS_E_ERROR_IN_FINISHED_PACKET"))
  ((:gnutls-e-unexpected-handshake-packet "GNUTLS_E_UNEXPECTED_HANDSHAKE_PACKET"))
  ((:gnutls-e-unknown-cipher-suite "GNUTLS_E_UNKNOWN_CIPHER_SUITE"))
  ((:gnutls-e-unwanted-algorithm "GNUTLS_E_UNWANTED_ALGORITHM"))
  ((:gnutls-e-mpi-scan-failed "GNUTLS_E_MPI_SCAN_FAILED"))
  ((:gnutls-e-decryption-failed "GNUTLS_E_DECRYPTION_FAILED"))
  ((:gnutls-e-memory-error "GNUTLS_E_MEMORY_ERROR"))
  ((:gnutls-e-decompression-failed "GNUTLS_E_DECOMPRESSION_FAILED"))
  ((:gnutls-e-compression-failed "GNUTLS_E_COMPRESSION_FAILED"))
  ((:gnutls-e-again "GNUTLS_E_AGAIN"))
  ((:gnutls-e-expired "GNUTLS_E_EXPIRED"))
  ((:gnutls-e-db-error "GNUTLS_E_DB_ERROR"))
  ((:gnutls-e-srp-pwd-error "GNUTLS_E_SRP_PWD_ERROR"))
  ((:gnutls-e-insufficient-credentials "GNUTLS_E_INSUFFICIENT_CREDENTIALS"))
  ((:gnutls-e-hash-failed "GNUTLS_E_HASH_FAILED"))
  ((:gnutls-e-base64-decoding-error "GNUTLS_E_BASE64_DECODING_ERROR"))
  ((:gnutls-e-mpi-print-failed "GNUTLS_E_MPI_PRINT_FAILED"))
  ((:gnutls-e-rehandshake "GNUTLS_E_REHANDSHAKE"))
  ((:gnutls-e-got-application-data "GNUTLS_E_GOT_APPLICATION_DATA"))
  ((:gnutls-e-record-limit-reached "GNUTLS_E_RECORD_LIMIT_REACHED"))
  ((:gnutls-e-encryption-failed "GNUTLS_E_ENCRYPTION_FAILED"))
  ((:gnutls-e-pk-encryption-failed "GNUTLS_E_PK_ENCRYPTION_FAILED"))
  ((:gnutls-e-pk-decryption-failed "GNUTLS_E_PK_DECRYPTION_FAILED"))
  ((:gnutls-e-pk-sign-failed "GNUTLS_E_PK_SIGN_FAILED"))
  ((:gnutls-e-x509-unsupported-critical-extension "GNUTLS_E_X509_UNSUPPORTED_CRITICAL_EXTENSION"))
  ((:gnutls-e-key-usage-violation "GNUTLS_E_KEY_USAGE_VIOLATION"))
  ((:gnutls-e-no-certificate-found "GNUTLS_E_NO_CERTIFICATE_FOUND"))
  ((:gnutls-e-invalid-request "GNUTLS_E_INVALID_REQUEST"))
  ((:gnutls-e-short-memory-buffer "GNUTLS_E_SHORT_MEMORY_BUFFER"))
  ((:gnutls-e-interrupted "GNUTLS_E_INTERRUPTED"))
  ((:gnutls-e-push-error "GNUTLS_E_PUSH_ERROR"))
  ((:gnutls-e-pull-error "GNUTLS_E_PULL_ERROR"))
  ((:gnutls-e-received-illegal-parameter "GNUTLS_E_RECEIVED_ILLEGAL_PARAMETER"))
  ((:gnutls-e-requested-data-not-available "GNUTLS_E_REQUESTED_DATA_NOT_AVAILABLE"))
  ((:gnutls-e-pkcs1-wrong-pad "GNUTLS_E_PKCS1_WRONG_PAD"))
  ((:gnutls-e-received-illegal-extension "GNUTLS_E_RECEIVED_ILLEGAL_EXTENSION"))
  ((:gnutls-e-internal-error "GNUTLS_E_INTERNAL_ERROR"))
  ((:gnutls-e-dh-prime-unacceptable "GNUTLS_E_DH_PRIME_UNACCEPTABLE"))
  ((:gnutls-e-file-error "GNUTLS_E_FILE_ERROR"))
  ((:gnutls-e-too-many-empty-packets "GNUTLS_E_TOO_MANY_EMPTY_PACKETS"))
  ((:gnutls-e-unknown-pk-algorithm "GNUTLS_E_UNKNOWN_PK_ALGORITHM"))
  ((:gnutls-e-init-libextra "GNUTLS_E_INIT_LIBEXTRA"))
  ((:gnutls-e-library-version-mismatch "GNUTLS_E_LIBRARY_VERSION_MISMATCH"))
  ((:gnutls-e-no-temporary-rsa-params "GNUTLS_E_NO_TEMPORARY_RSA_PARAMS"))
  ((:gnutls-e-lzo-init-failed "GNUTLS_E_LZO_INIT_FAILED"))
  ((:gnutls-e-no-compression-algorithms "GNUTLS_E_NO_COMPRESSION_ALGORITHMS"))
  ((:gnutls-e-no-cipher-suites "GNUTLS_E_NO_CIPHER_SUITES"))
  ((:gnutls-e-openpgp-getkey-failed "GNUTLS_E_OPENPGP_GETKEY_FAILED"))
  ((:gnutls-e-pk-sig-verify-failed "GNUTLS_E_PK_SIG_VERIFY_FAILED"))
  ((:gnutls-e-illegal-srp-username "GNUTLS_E_ILLEGAL_SRP_USERNAME"))
  ((:gnutls-e-srp-pwd-parsing-error "GNUTLS_E_SRP_PWD_PARSING_ERROR"))
  ((:gnutls-e-no-temporary-dh-params "GNUTLS_E_NO_TEMPORARY_DH_PARAMS"))
  ((:gnutls-e-asn1-element-not-found "GNUTLS_E_ASN1_ELEMENT_NOT_FOUND"))
  ((:gnutls-e-asn1-identifier-not-found "GNUTLS_E_ASN1_IDENTIFIER_NOT_FOUND"))
  ((:gnutls-e-asn1-der-error "GNUTLS_E_ASN1_DER_ERROR"))
  ((:gnutls-e-asn1-value-not-found "GNUTLS_E_ASN1_VALUE_NOT_FOUND"))
  ((:gnutls-e-asn1-generic-error "GNUTLS_E_ASN1_GENERIC_ERROR"))
  ((:gnutls-e-asn1-value-not-valid "GNUTLS_E_ASN1_VALUE_NOT_VALID"))
  ((:gnutls-e-asn1-tag-error "GNUTLS_E_ASN1_TAG_ERROR"))
  ((:gnutls-e-asn1-tag-implicit "GNUTLS_E_ASN1_TAG_IMPLICIT"))
  ((:gnutls-e-asn1-type-any-error "GNUTLS_E_ASN1_TYPE_ANY_ERROR"))
  ((:gnutls-e-asn1-syntax-error "GNUTLS_E_ASN1_SYNTAX_ERROR"))
  ((:gnutls-e-asn1-der-overflow "GNUTLS_E_ASN1_DER_OVERFLOW"))
  ((:gnutls-e-openpgp-uid-revoked "GNUTLS_E_OPENPGP_UID_REVOKED"))
  ((:gnutls-e-certificate-error "GNUTLS_E_CERTIFICATE_ERROR"))
  ((:gnutls-e-certificate-key-mismatch "GNUTLS_E_CERTIFICATE_KEY_MISMATCH"))
  ((:gnutls-e-unsupported-certificate-type "GNUTLS_E_UNSUPPORTED_CERTIFICATE_TYPE"))
  ((:gnutls-e-x509-unknown-san "GNUTLS_E_X509_UNKNOWN_SAN"))
  ((:gnutls-e-openpgp-fingerprint-unsupported "GNUTLS_E_OPENPGP_FINGERPRINT_UNSUPPORTED"))
  ((:gnutls-e-x509-unsupported-attribute "GNUTLS_E_X509_UNSUPPORTED_ATTRIBUTE"))
  ((:gnutls-e-unknown-hash-algorithm "GNUTLS_E_UNKNOWN_HASH_ALGORITHM"))
  ((:gnutls-e-unknown-pkcs-content-type "GNUTLS_E_UNKNOWN_PKCS_CONTENT_TYPE"))
  ((:gnutls-e-unknown-pkcs-bag-type "GNUTLS_E_UNKNOWN_PKCS_BAG_TYPE"))
  ((:gnutls-e-invalid-password "GNUTLS_E_INVALID_PASSWORD"))
  ((:gnutls-e-mac-verify-failed "GNUTLS_E_MAC_VERIFY_FAILED"))
  ((:gnutls-e-constraint-error "GNUTLS_E_CONSTRAINT_ERROR"))
  ((:gnutls-e-warning-ia-iphf-received "GNUTLS_E_WARNING_IA_IPHF_RECEIVED"))
  ((:gnutls-e-warning-ia-fphf-received "GNUTLS_E_WARNING_IA_FPHF_RECEIVED"))
  ((:gnutls-e-ia-verify-failed "GNUTLS_E_IA_VERIFY_FAILED"))
  ((:gnutls-e-unknown-algorithm "GNUTLS_E_UNKNOWN_ALGORITHM"))
  ((:gnutls-e-base64-encoding-error "GNUTLS_E_BASE64_ENCODING_ERROR"))
  ((:gnutls-e-incompatible-crypto-library "GNUTLS_E_INCOMPATIBLE_CRYPTO_LIBRARY"))
  ((:gnutls-e-incompatible-libtasn1-library "GNUTLS_E_INCOMPATIBLE_LIBTASN1_LIBRARY"))
  ((:gnutls-e-openpgp-keyring-error "GNUTLS_E_OPENPGP_KEYRING_ERROR"))
  ((:gnutls-e-x509-unsupported-oid "GNUTLS_E_X509_UNSUPPORTED_OID"))
  ((:gnutls-e-random-failed "GNUTLS_E_RANDOM_FAILED"))
  ((:gnutls-e-base64-unexpected-header-error "GNUTLS_E_BASE64_UNEXPECTED_HEADER_ERROR"))
  ((:gnutls-e-unimplemented-feature "GNUTLS_E_UNIMPLEMENTED_FEATURE")))
