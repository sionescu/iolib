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
  ((:unknown-compression-algorithm "GNUTLS_E_UNKNOWN_COMPRESSION_ALGORITHM"))
  ((:unknown-cipher-type "GNUTLS_E_UNKNOWN_CIPHER_TYPE"))
  ((:large-packet "GNUTLS_E_LARGE_PACKET"))
  ((:unsupported-version-packet "GNUTLS_E_UNSUPPORTED_VERSION_PACKET"))
  ((:unexpected-packet-length "GNUTLS_E_UNEXPECTED_PACKET_LENGTH"))
  ((:invalid-session "GNUTLS_E_INVALID_SESSION"))
  ((:fatal-alert-received "GNUTLS_E_FATAL_ALERT_RECEIVED"))
  ((:unexpected-packet "GNUTLS_E_UNEXPECTED_PACKET"))
  ((:warning-alert-received "GNUTLS_E_WARNING_ALERT_RECEIVED"))
  ((:error-in-finished-packet "GNUTLS_E_ERROR_IN_FINISHED_PACKET"))
  ((:unexpected-handshake-packet "GNUTLS_E_UNEXPECTED_HANDSHAKE_PACKET"))
  ((:unknown-cipher-suite "GNUTLS_E_UNKNOWN_CIPHER_SUITE"))
  ((:unwanted-algorithm "GNUTLS_E_UNWANTED_ALGORITHM"))
  ((:mpi-scan-failed "GNUTLS_E_MPI_SCAN_FAILED"))
  ((:decryption-failed "GNUTLS_E_DECRYPTION_FAILED"))
  ((:memory-error "GNUTLS_E_MEMORY_ERROR"))
  ((:decompression-failed "GNUTLS_E_DECOMPRESSION_FAILED"))
  ((:compression-failed "GNUTLS_E_COMPRESSION_FAILED"))
  ((:again "GNUTLS_E_AGAIN"))
  ((:expired "GNUTLS_E_EXPIRED"))
  ((:db-error "GNUTLS_E_DB_ERROR"))
  ((:srp-pwd-error "GNUTLS_E_SRP_PWD_ERROR"))
  ((:insufficient-credentials "GNUTLS_E_INSUFFICIENT_CREDENTIALS"))
  ((:hash-failed "GNUTLS_E_HASH_FAILED"))
  ((:base64-decoding-error "GNUTLS_E_BASE64_DECODING_ERROR"))
  ((:mpi-print-failed "GNUTLS_E_MPI_PRINT_FAILED"))
  ((:rehandshake "GNUTLS_E_REHANDSHAKE"))
  ((:got-application-data "GNUTLS_E_GOT_APPLICATION_DATA"))
  ((:record-limit-reached "GNUTLS_E_RECORD_LIMIT_REACHED"))
  ((:encryption-failed "GNUTLS_E_ENCRYPTION_FAILED"))
  ((:pk-encryption-failed "GNUTLS_E_PK_ENCRYPTION_FAILED"))
  ((:pk-decryption-failed "GNUTLS_E_PK_DECRYPTION_FAILED"))
  ((:pk-sign-failed "GNUTLS_E_PK_SIGN_FAILED"))
  ((:x509-unsupported-critical-extension "GNUTLS_E_X509_UNSUPPORTED_CRITICAL_EXTENSION"))
  ((:key-usage-violation "GNUTLS_E_KEY_USAGE_VIOLATION"))
  ((:no-certificate-found "GNUTLS_E_NO_CERTIFICATE_FOUND"))
  ((:invalid-request "GNUTLS_E_INVALID_REQUEST"))
  ((:short-memory-buffer "GNUTLS_E_SHORT_MEMORY_BUFFER"))
  ((:interrupted "GNUTLS_E_INTERRUPTED"))
  ((:push-error "GNUTLS_E_PUSH_ERROR"))
  ((:pull-error "GNUTLS_E_PULL_ERROR"))
  ((:received-illegal-parameter "GNUTLS_E_RECEIVED_ILLEGAL_PARAMETER"))
  ((:requested-data-not-available "GNUTLS_E_REQUESTED_DATA_NOT_AVAILABLE"))
  ((:pkcs1-wrong-pad "GNUTLS_E_PKCS1_WRONG_PAD"))
  ((:received-illegal-extension "GNUTLS_E_RECEIVED_ILLEGAL_EXTENSION"))
  ((:internal-error "GNUTLS_E_INTERNAL_ERROR"))
  ((:dh-prime-unacceptable "GNUTLS_E_DH_PRIME_UNACCEPTABLE"))
  ((:file-error "GNUTLS_E_FILE_ERROR"))
  ((:too-many-empty-packets "GNUTLS_E_TOO_MANY_EMPTY_PACKETS"))
  ((:unknown-pk-algorithm "GNUTLS_E_UNKNOWN_PK_ALGORITHM"))
  ((:init-libextra "GNUTLS_E_INIT_LIBEXTRA"))
  ((:library-version-mismatch "GNUTLS_E_LIBRARY_VERSION_MISMATCH"))
  ((:no-temporary-rsa-params "GNUTLS_E_NO_TEMPORARY_RSA_PARAMS"))
  ((:lzo-init-failed "GNUTLS_E_LZO_INIT_FAILED"))
  ((:no-compression-algorithms "GNUTLS_E_NO_COMPRESSION_ALGORITHMS"))
  ((:no-cipher-suites "GNUTLS_E_NO_CIPHER_SUITES"))
  ((:openpgp-getkey-failed "GNUTLS_E_OPENPGP_GETKEY_FAILED"))
  ((:pk-sig-verify-failed "GNUTLS_E_PK_SIG_VERIFY_FAILED"))
  ((:illegal-srp-username "GNUTLS_E_ILLEGAL_SRP_USERNAME"))
  ((:srp-pwd-parsing-error "GNUTLS_E_SRP_PWD_PARSING_ERROR"))
  ((:no-temporary-dh-params "GNUTLS_E_NO_TEMPORARY_DH_PARAMS"))
  ((:asn1-element-not-found "GNUTLS_E_ASN1_ELEMENT_NOT_FOUND"))
  ((:asn1-identifier-not-found "GNUTLS_E_ASN1_IDENTIFIER_NOT_FOUND"))
  ((:asn1-der-error "GNUTLS_E_ASN1_DER_ERROR"))
  ((:asn1-value-not-found "GNUTLS_E_ASN1_VALUE_NOT_FOUND"))
  ((:asn1-generic-error "GNUTLS_E_ASN1_GENERIC_ERROR"))
  ((:asn1-value-not-valid "GNUTLS_E_ASN1_VALUE_NOT_VALID"))
  ((:asn1-tag-error "GNUTLS_E_ASN1_TAG_ERROR"))
  ((:asn1-tag-implicit "GNUTLS_E_ASN1_TAG_IMPLICIT"))
  ((:asn1-type-any-error "GNUTLS_E_ASN1_TYPE_ANY_ERROR"))
  ((:asn1-syntax-error "GNUTLS_E_ASN1_SYNTAX_ERROR"))
  ((:asn1-der-overflow "GNUTLS_E_ASN1_DER_OVERFLOW"))
  ((:openpgp-uid-revoked "GNUTLS_E_OPENPGP_UID_REVOKED"))
  ((:certificate-error "GNUTLS_E_CERTIFICATE_ERROR"))
  ((:certificate-key-mismatch "GNUTLS_E_CERTIFICATE_KEY_MISMATCH"))
  ((:unsupported-certificate-type "GNUTLS_E_UNSUPPORTED_CERTIFICATE_TYPE"))
  ((:x509-unknown-san "GNUTLS_E_X509_UNKNOWN_SAN"))
  ((:openpgp-fingerprint-unsupported "GNUTLS_E_OPENPGP_FINGERPRINT_UNSUPPORTED"))
  ((:x509-unsupported-attribute "GNUTLS_E_X509_UNSUPPORTED_ATTRIBUTE"))
  ((:unknown-hash-algorithm "GNUTLS_E_UNKNOWN_HASH_ALGORITHM"))
  ((:unknown-pkcs-content-type "GNUTLS_E_UNKNOWN_PKCS_CONTENT_TYPE"))
  ((:unknown-pkcs-bag-type "GNUTLS_E_UNKNOWN_PKCS_BAG_TYPE"))
  ((:invalid-password "GNUTLS_E_INVALID_PASSWORD"))
  ((:mac-verify-failed "GNUTLS_E_MAC_VERIFY_FAILED"))
  ((:constraint-error "GNUTLS_E_CONSTRAINT_ERROR"))
  ((:warning-ia-iphf-received "GNUTLS_E_WARNING_IA_IPHF_RECEIVED"))
  ((:warning-ia-fphf-received "GNUTLS_E_WARNING_IA_FPHF_RECEIVED"))
  ((:ia-verify-failed "GNUTLS_E_IA_VERIFY_FAILED"))
  ((:unknown-algorithm "GNUTLS_E_UNKNOWN_ALGORITHM"))
  ((:base64-encoding-error "GNUTLS_E_BASE64_ENCODING_ERROR"))
  ((:incompatible-crypto-library "GNUTLS_E_INCOMPATIBLE_CRYPTO_LIBRARY"))
  ((:incompatible-libtasn1-library "GNUTLS_E_INCOMPATIBLE_LIBTASN1_LIBRARY"))
  ((:openpgp-keyring-error "GNUTLS_E_OPENPGP_KEYRING_ERROR"))
  ((:x509-unsupported-oid "GNUTLS_E_X509_UNSUPPORTED_OID"))
  ((:random-failed "GNUTLS_E_RANDOM_FAILED"))
  ((:base64-unexpected-header-error "GNUTLS_E_BASE64_UNEXPECTED_HEADER_ERROR"))
  ((:unimplemented-feature "GNUTLS_E_UNIMPLEMENTED_FEATURE")))

(cenum gnutls-connection-end
  ((:server "GNUTLS_SERVER"))
  ((:client "GNUTLS_CLIENT")))

(cenum gnutls-close-request
  ((:shut-rdwr "GNUTLS_SHUT_RDWR"))
  ((:shut-wr   "GNUTLS_SHUT_WR")))

(cenum gnutls-credentials-type
  ((:certificate "GNUTLS_CRD_CERTIFICATE"))
  ((:anon "GNUTLS_CRD_ANON"))
  ((:srp "GNUTLS_CRD_SRP"))
  ((:psk "GNUTLS_CRD_PSK"))
  ((:ia "GNUTLS_CRD_IA")))
