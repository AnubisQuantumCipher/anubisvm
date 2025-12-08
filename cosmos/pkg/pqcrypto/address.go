// Address derivation for AegisVM
//
// Implements AAS-001 v3.1 - Aegis Address Standard
// Canonical format: mldsa87:network:type:chunked_payload-checksum
// Example: mldsa87:main:u:dztd939y-b16ybkqa-12a55nb1-zyb1f50k-nn08yc11-fskf8sfs-0vsg-bg0bc
//
// This is a NIST Level 5 quantum-resistant address system using ML-DSA-87.

package pqcrypto

/*
#include "aegisvm.h"
#include <stdlib.h>
*/
import "C"

import (
	"errors"
	"unsafe"
)

// Address constants for AAS-001 v3.1
const (
	// AccountIDSize is the size of the account ID in bytes (32 bytes = 256 bits)
	AccountIDSize = 32

	// MaxAddressStringSize is the maximum length of a canonical address string
	MaxAddressStringSize = 96

	// MinAddressStringSize is the minimum length of a canonical address string
	MinAddressStringSize = 78
)

// NetworkType represents the network for the address
type NetworkType int

const (
	NetworkMain    NetworkType = C.AEGIS_NET_MAIN
	NetworkTest    NetworkType = C.AEGIS_NET_TEST
	NetworkDev     NetworkType = C.AEGIS_NET_DEV
	NetworkLab     NetworkType = C.AEGIS_NET_LAB
	NetworkStaging NetworkType = C.AEGIS_NET_STAGING
)

// AddressType represents the entity type for the address
type AddressType byte

const (
	AddressTypeUser      AddressType = 'u'
	AddressTypeContract  AddressType = 'c'
	AddressTypeValidator AddressType = 'v'
	AddressTypeSystem    AddressType = 's'
)

// Address errors
var (
	ErrInvalidAddress = errors.New("pqcrypto: invalid address format")
	ErrAddressParse   = errors.New("pqcrypto: failed to parse address")
	ErrAddressFormat  = errors.New("pqcrypto: failed to format address")
)

// AccountID represents a 32-byte account identifier derived from ML-DSA-87 public key
type AccountID [AccountIDSize]byte

// Address represents a full AAS-001 v3.1 address with all components
type Address struct {
	AccountID AccountID
	Network   NetworkType
	Type      AddressType
}

// FormatAddress formats a public key to canonical AAS-001 v3.1 address string.
// Returns the canonical address like: mldsa87:main:u:dztd939y-b16ybkqa-12a55nb1-...
func FormatAddress(pk PublicKey, network NetworkType, addrType AddressType) (string, error) {
	output := make([]byte, MaxAddressStringSize)
	var length C.size_t

	result := C.aegis_format_address(
		(*C.char)(unsafe.Pointer(&output[0])),
		&length,
		(*C.uint8_t)(unsafe.Pointer(&pk[0])),
		C.aegis_network_t(network),
		C.aegis_addr_type_t(addrType),
	)

	if result != C.AEGIS_OK {
		return "", ErrAddressFormat
	}

	return string(output[:length]), nil
}

// FormatUserAddress formats a public key to a user address on the main network.
// This is the default address format for user accounts.
func FormatUserAddress(pk PublicKey) (string, error) {
	return FormatAddress(pk, NetworkMain, AddressTypeUser)
}

// FormatContractAddress formats a public key to a contract address.
func FormatContractAddress(pk PublicKey, network NetworkType) (string, error) {
	return FormatAddress(pk, network, AddressTypeContract)
}

// FormatValidatorAddress formats a public key to a validator address.
func FormatValidatorAddress(pk PublicKey, network NetworkType) (string, error) {
	return FormatAddress(pk, network, AddressTypeValidator)
}

// ParseAddress parses a canonical AAS-001 v3.1 address string.
// Returns the parsed address components or an error if invalid.
func ParseAddress(addrStr string) (*Address, error) {
	if len(addrStr) < MinAddressStringSize || len(addrStr) > MaxAddressStringSize {
		return nil, ErrInvalidAddress
	}

	var accountID AccountID
	var network C.aegis_network_t
	var addrType C.aegis_addr_type_t

	cStr := C.CString(addrStr)
	defer C.free(unsafe.Pointer(cStr))

	result := C.aegis_parse_address(
		cStr,
		C.size_t(len(addrStr)),
		(*C.aegis_address_t)(unsafe.Pointer(&accountID[0])),
		&network,
		&addrType,
	)

	if result != C.AEGIS_OK {
		return nil, ErrAddressParse
	}

	return &Address{
		AccountID: accountID,
		Network:   NetworkType(network),
		Type:      AddressType(addrType),
	}, nil
}

// String returns the canonical address string representation.
// Note: This requires the original public key to regenerate.
// For display, use FormatAddress with the public key.
func (a *Address) String() string {
	// Return a short representation for debugging
	return "Address{" + string(a.Type) + ":" + networkString(a.Network) + "}"
}

// Bytes returns the 32-byte account ID.
func (a *Address) Bytes() []byte {
	return a.AccountID[:]
}

// IsUser returns true if this is a user address.
func (a *Address) IsUser() bool {
	return a.Type == AddressTypeUser
}

// IsContract returns true if this is a contract address.
func (a *Address) IsContract() bool {
	return a.Type == AddressTypeContract
}

// IsValidator returns true if this is a validator address.
func (a *Address) IsValidator() bool {
	return a.Type == AddressTypeValidator
}

// IsSystem returns true if this is a system address.
func (a *Address) IsSystem() bool {
	return a.Type == AddressTypeSystem
}

// IsMainnet returns true if this address is on the main network.
func (a *Address) IsMainnet() bool {
	return a.Network == NetworkMain
}

// Equal returns true if two addresses are equal.
func (a *Address) Equal(other *Address) bool {
	if other == nil {
		return false
	}
	return a.AccountID == other.AccountID &&
		a.Network == other.Network &&
		a.Type == other.Type
}

// networkString returns the string representation of a network type.
func networkString(n NetworkType) string {
	switch n {
	case NetworkMain:
		return "main"
	case NetworkTest:
		return "test"
	case NetworkDev:
		return "dev"
	case NetworkLab:
		return "lab"
	case NetworkStaging:
		return "staging"
	default:
		return "unknown"
	}
}

// DeriveAccountID derives a 32-byte account ID from an ML-DSA-87 public key.
// Uses domain-separated SHA3-256 hashing per AAS-001 v3.1.
func DeriveAccountID(pk PublicKey, addrType AddressType) (AccountID, error) {
	var accountID AccountID

	result := C.aegis_derive_address(
		(*C.aegis_address_t)(unsafe.Pointer(&accountID[0])),
		(*C.uint8_t)(unsafe.Pointer(&pk[0])),
		C.aegis_addr_type_t(addrType),
	)

	if result != C.AEGIS_OK {
		return accountID, ErrKeyGenFailed
	}

	return accountID, nil
}

// ValidateAddress checks if an address string is valid per AAS-001 v3.1.
func ValidateAddress(addrStr string) bool {
	_, err := ParseAddress(addrStr)
	return err == nil
}
