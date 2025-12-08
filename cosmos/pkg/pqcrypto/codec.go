// Package pqcrypto provides AAS-001 v3.1 address codec for Cosmos SDK.
//
// This implements the Cosmos SDK address.Codec interface using the
// Aegis Address Standard (AAS-001 v3.1) for quantum-resistant addresses.
//
// Canonical format: mldsa87:<network>:<type>:<chunked_payload>-<checksum>
// Example: mldsa87:main:u:qr7zy5kx-mjgpv4wc-8h2d6nft-3se09ax7-k5n2p8rv-wd4h7mcq-j9f3-v8k4n

package pqcrypto

import (
	"errors"
	"fmt"
	"strings"

	"golang.org/x/crypto/sha3"
)

const (
	// Algorithm tag for ML-DSA-87
	AlgorithmTag = "mldsa87"

	// Crockford Base32 alphabet (excludes i, l, o, u)
	crockfordAlphabet = "0123456789abcdefghjkmnpqrstvwxyz"

	// Address component sizes
	payloadChars   = 52 // Base32 chars for 32-byte account ID
	checksumChars  = 5  // Base32 chars for 24-bit checksum
	checksumBytes  = 3  // 24 bits = 3 bytes

	// Chunk pattern: 8-8-8-8-8-8-8-4
	chunkPattern = "8-8-8-8-8-8-8-4"
)

// Crockford Base32 decoding table
var crockfordDecodeTable [256]byte

func init() {
	// Initialize decode table with invalid marker
	for i := range crockfordDecodeTable {
		crockfordDecodeTable[i] = 0xFF
	}

	// Map valid characters
	for i, c := range crockfordAlphabet {
		crockfordDecodeTable[c] = byte(i)
		// Also accept uppercase
		if c >= 'a' && c <= 'z' {
			crockfordDecodeTable[c-32] = byte(i)
		}
	}
}

// AASAddressCodec implements address.Codec for AAS-001 v3.1 user addresses.
// This is the primary address codec for user (externally owned) accounts.
type AASAddressCodec struct {
	Network NetworkType
}

// NewAASAddressCodec creates a new AAS-001 address codec for user addresses.
func NewAASAddressCodec(network NetworkType) *AASAddressCodec {
	return &AASAddressCodec{Network: network}
}

// NewMainnetAddressCodec creates a codec for mainnet user addresses.
func NewMainnetAddressCodec() *AASAddressCodec {
	return NewAASAddressCodec(NetworkMain)
}

// NewTestnetAddressCodec creates a codec for testnet user addresses.
func NewTestnetAddressCodec() *AASAddressCodec {
	return NewAASAddressCodec(NetworkTest)
}

// StringToBytes decodes an AAS-001 address string to bytes.
// Returns 32 bytes for user/contract/validator addresses, 20 bytes for system module addresses.
func (c *AASAddressCodec) StringToBytes(text string) ([]byte, error) {
	// Normalize to lowercase
	text = strings.ToLower(strings.TrimSpace(text))

	// Split by colons
	parts := strings.Split(text, ":")
	if len(parts) != 4 {
		return nil, fmt.Errorf("invalid AAS-001 address: expected 4 parts separated by ':', got %d", len(parts))
	}

	algo := parts[0]
	network := parts[1]
	addrType := parts[2]
	payloadAndChecksum := parts[3]

	// Validate algorithm
	if algo != AlgorithmTag {
		return nil, fmt.Errorf("invalid algorithm: expected '%s', got '%s'", AlgorithmTag, algo)
	}

	// Validate network
	if !isValidNetwork(network) {
		return nil, fmt.Errorf("invalid network: '%s'", network)
	}

	// Validate type
	if !isValidAddressType(addrType) {
		return nil, fmt.Errorf("invalid address type: '%s'", addrType)
	}

	// Extract payload and checksum (last '-' separates checksum)
	lastDash := strings.LastIndex(payloadAndChecksum, "-")
	if lastDash == -1 {
		return nil, errors.New("invalid AAS-001 address: missing checksum separator")
	}

	payloadChunked := payloadAndChecksum[:lastDash]
	checksumGiven := payloadAndChecksum[lastDash+1:]

	if len(checksumGiven) != checksumChars {
		return nil, fmt.Errorf("invalid checksum length: expected %d, got %d", checksumChars, len(checksumGiven))
	}

	// Remove dashes from payload
	payloadRaw := strings.ReplaceAll(payloadChunked, "-", "")
	if len(payloadRaw) != payloadChars {
		return nil, fmt.Errorf("invalid payload length: expected %d, got %d", payloadChars, len(payloadRaw))
	}

	// Validate Base32 characters
	if err := validateBase32(payloadRaw); err != nil {
		return nil, fmt.Errorf("invalid payload: %w", err)
	}
	if err := validateBase32(checksumGiven); err != nil {
		return nil, fmt.Errorf("invalid checksum: %w", err)
	}

	// Verify checksum
	preimage := fmt.Sprintf("%s:%s:%s:%s", algo, network, addrType, payloadRaw)
	expectedChecksum := computeChecksum(preimage)
	if checksumGiven != expectedChecksum {
		return nil, fmt.Errorf("checksum verification failed: expected '%s', got '%s'", expectedChecksum, checksumGiven)
	}

	// Decode account ID
	accountID, err := crockfordBase32Decode(payloadRaw)
	if err != nil {
		return nil, fmt.Errorf("failed to decode account ID: %w", err)
	}

	if len(accountID) != AccountIDSize {
		return nil, fmt.Errorf("invalid account ID size: expected %d, got %d", AccountIDSize, len(accountID))
	}

	// For system module addresses (type 's'), return only the first 20 bytes
	// since module addresses in Cosmos SDK are 20 bytes
	if addrType == "s" {
		return accountID[:20], nil
	}

	return accountID, nil
}

// BytesToString encodes bytes to an address string.
// Supports both 32-byte AAS-001 addresses and 20-byte legacy module addresses.
func (c *AASAddressCodec) BytesToString(bz []byte) (string, error) {
	switch len(bz) {
	case AccountIDSize: // 32 bytes - AAS-001 format
		return formatAASAddress(bz, c.Network, AddressTypeUser)
	case 20: // Legacy 20-byte module address - format as system module
		// Pad to 32 bytes with zeros for consistent format
		padded := make([]byte, AccountIDSize)
		copy(padded, bz)
		return formatAASAddress(padded, c.Network, AddressTypeSystem)
	default:
		return "", fmt.Errorf("invalid account ID size: expected %d or 20 bytes, got %d", AccountIDSize, len(bz))
	}
}

// LengthEquals implements the Codec interface check for address length.
// Accepts both 32-byte AAS-001 addresses and 20-byte legacy module addresses.
func (c *AASAddressCodec) LengthEquals(bz []byte) bool {
	return len(bz) == AccountIDSize || len(bz) == 20
}

// AASValidatorAddressCodec implements address.Codec for validator addresses.
type AASValidatorAddressCodec struct {
	Network NetworkType
}

// NewAASValidatorAddressCodec creates a new AAS-001 address codec for validators.
func NewAASValidatorAddressCodec(network NetworkType) *AASValidatorAddressCodec {
	return &AASValidatorAddressCodec{Network: network}
}

// StringToBytes decodes an AAS-001 validator address string to the 32-byte account ID.
func (c *AASValidatorAddressCodec) StringToBytes(text string) ([]byte, error) {
	// Use the same parsing logic, but validate it's a validator address
	codec := &AASAddressCodec{Network: c.Network}
	bz, err := codec.parseAddress(text, AddressTypeValidator)
	if err != nil {
		return nil, err
	}
	return bz, nil
}

// BytesToString encodes a 32-byte account ID to an AAS-001 validator address string.
func (c *AASValidatorAddressCodec) BytesToString(bz []byte) (string, error) {
	if len(bz) != AccountIDSize {
		return "", fmt.Errorf("invalid account ID size: expected %d bytes, got %d", AccountIDSize, len(bz))
	}

	return formatAASAddress(bz, c.Network, AddressTypeValidator)
}

// LengthEquals implements the Codec interface check for address length
func (c *AASValidatorAddressCodec) LengthEquals(bz []byte) bool {
	return len(bz) == AccountIDSize
}

// AASConsensusAddressCodec implements address.Codec for consensus (valcons) addresses.
// Uses the same format as validator addresses with type 'v'.
type AASConsensusAddressCodec struct {
	Network NetworkType
}

// NewAASConsensusAddressCodec creates a new AAS-001 address codec for consensus nodes.
func NewAASConsensusAddressCodec(network NetworkType) *AASConsensusAddressCodec {
	return &AASConsensusAddressCodec{Network: network}
}

// StringToBytes decodes an AAS-001 consensus address string to the 32-byte account ID.
func (c *AASConsensusAddressCodec) StringToBytes(text string) ([]byte, error) {
	codec := &AASAddressCodec{Network: c.Network}
	return codec.parseAddress(text, AddressTypeValidator)
}

// BytesToString encodes a 32-byte account ID to an AAS-001 consensus address string.
func (c *AASConsensusAddressCodec) BytesToString(bz []byte) (string, error) {
	if len(bz) != AccountIDSize {
		return "", fmt.Errorf("invalid account ID size: expected %d bytes, got %d", AccountIDSize, len(bz))
	}

	return formatAASAddress(bz, c.Network, AddressTypeValidator)
}

// LengthEquals implements the Codec interface check for address length
func (c *AASConsensusAddressCodec) LengthEquals(bz []byte) bool {
	return len(bz) == AccountIDSize
}

// parseAddress parses an AAS-001 address expecting a specific type.
func (c *AASAddressCodec) parseAddress(text string, expectedType AddressType) ([]byte, error) {
	text = strings.ToLower(strings.TrimSpace(text))

	parts := strings.Split(text, ":")
	if len(parts) != 4 {
		return nil, fmt.Errorf("invalid AAS-001 address: expected 4 parts, got %d", len(parts))
	}

	algo := parts[0]
	network := parts[1]
	addrType := parts[2]
	payloadAndChecksum := parts[3]

	if algo != AlgorithmTag {
		return nil, fmt.Errorf("invalid algorithm: expected '%s', got '%s'", AlgorithmTag, algo)
	}

	if !isValidNetwork(network) {
		return nil, fmt.Errorf("invalid network: '%s'", network)
	}

	if addrType != string(expectedType) {
		return nil, fmt.Errorf("address type mismatch: expected '%c', got '%s'", expectedType, addrType)
	}

	lastDash := strings.LastIndex(payloadAndChecksum, "-")
	if lastDash == -1 {
		return nil, errors.New("invalid address: missing checksum separator")
	}

	payloadChunked := payloadAndChecksum[:lastDash]
	checksumGiven := payloadAndChecksum[lastDash+1:]

	if len(checksumGiven) != checksumChars {
		return nil, fmt.Errorf("invalid checksum length: expected %d, got %d", checksumChars, len(checksumGiven))
	}

	payloadRaw := strings.ReplaceAll(payloadChunked, "-", "")
	if len(payloadRaw) != payloadChars {
		return nil, fmt.Errorf("invalid payload length: expected %d, got %d", payloadChars, len(payloadRaw))
	}

	if err := validateBase32(payloadRaw); err != nil {
		return nil, fmt.Errorf("invalid payload: %w", err)
	}
	if err := validateBase32(checksumGiven); err != nil {
		return nil, fmt.Errorf("invalid checksum: %w", err)
	}

	preimage := fmt.Sprintf("%s:%s:%s:%s", algo, network, addrType, payloadRaw)
	expectedChecksum := computeChecksum(preimage)
	if checksumGiven != expectedChecksum {
		return nil, fmt.Errorf("checksum verification failed: expected '%s', got '%s'", expectedChecksum, checksumGiven)
	}

	accountID, err := crockfordBase32Decode(payloadRaw)
	if err != nil {
		return nil, fmt.Errorf("failed to decode account ID: %w", err)
	}

	return accountID, nil
}

// formatAASAddress formats a 32-byte account ID into an AAS-001 canonical address.
func formatAASAddress(accountID []byte, network NetworkType, addrType AddressType) (string, error) {
	if len(accountID) != AccountIDSize {
		return "", fmt.Errorf("invalid account ID size: expected %d, got %d", AccountIDSize, len(accountID))
	}

	// Encode to Crockford Base32
	payloadRaw := crockfordBase32Encode(accountID)
	if len(payloadRaw) != payloadChars {
		return "", fmt.Errorf("unexpected payload length: expected %d, got %d", payloadChars, len(payloadRaw))
	}

	// Chunk the payload: 8-8-8-8-8-8-8-4
	chunks := []string{
		payloadRaw[0:8],
		payloadRaw[8:16],
		payloadRaw[16:24],
		payloadRaw[24:32],
		payloadRaw[32:40],
		payloadRaw[40:48],
		payloadRaw[48:52],
	}
	payloadChunked := strings.Join(chunks, "-")

	// Compute checksum
	networkStr := networkToString(network)
	preimage := fmt.Sprintf("%s:%s:%c:%s", AlgorithmTag, networkStr, addrType, payloadRaw)
	checksum := computeChecksum(preimage)

	// Assemble final address
	return fmt.Sprintf("%s:%s:%c:%s-%s", AlgorithmTag, networkStr, addrType, payloadChunked, checksum), nil
}

// computeChecksum computes the 24-bit checksum for an AAS-001 preimage.
func computeChecksum(preimage string) string {
	hash := sha3.Sum256([]byte(preimage))
	checksumBytes := hash[:3] // First 3 bytes (24 bits)
	return crockfordBase32Encode(checksumBytes)
}

// crockfordBase32Encode encodes bytes to Crockford Base32.
func crockfordBase32Encode(data []byte) string {
	if len(data) == 0 {
		return ""
	}

	result := make([]byte, 0, (len(data)*8+4)/5)
	buffer := uint64(0)
	bitsInBuffer := 0

	for _, b := range data {
		buffer = (buffer << 8) | uint64(b)
		bitsInBuffer += 8

		for bitsInBuffer >= 5 {
			bitsInBuffer -= 5
			index := (buffer >> bitsInBuffer) & 0x1F
			result = append(result, crockfordAlphabet[index])
		}
	}

	// Handle remaining bits
	if bitsInBuffer > 0 {
		index := (buffer << (5 - bitsInBuffer)) & 0x1F
		result = append(result, crockfordAlphabet[index])
	}

	return string(result)
}

// crockfordBase32Decode decodes Crockford Base32 to bytes.
func crockfordBase32Decode(s string) ([]byte, error) {
	if len(s) == 0 {
		return nil, nil
	}

	// Calculate expected output size
	outputSize := len(s) * 5 / 8
	result := make([]byte, 0, outputSize)
	buffer := uint64(0)
	bitsInBuffer := 0

	for i := 0; i < len(s); i++ {
		c := s[i]
		val := crockfordDecodeTable[c]
		if val == 0xFF {
			return nil, fmt.Errorf("invalid character '%c' at position %d", c, i)
		}

		buffer = (buffer << 5) | uint64(val)
		bitsInBuffer += 5

		if bitsInBuffer >= 8 {
			bitsInBuffer -= 8
			result = append(result, byte(buffer>>bitsInBuffer))
		}
	}

	return result, nil
}

// validateBase32 checks if a string contains only valid Crockford Base32 characters.
func validateBase32(s string) error {
	for i, c := range s {
		if c > 255 || crockfordDecodeTable[byte(c)] == 0xFF {
			return fmt.Errorf("invalid character '%c' at position %d", c, i)
		}
	}
	return nil
}

// isValidNetwork checks if a network string is valid.
func isValidNetwork(network string) bool {
	switch network {
	case "main", "test", "dev", "lab", "staging":
		return true
	}
	// Check for numbered devnets (dev1-dev99)
	if len(network) >= 4 && len(network) <= 5 && strings.HasPrefix(network, "dev") {
		for i := 3; i < len(network); i++ {
			if network[i] < '0' || network[i] > '9' {
				return false
			}
		}
		return true
	}
	return false
}

// isValidAddressType checks if an address type is valid.
func isValidAddressType(addrType string) bool {
	if len(addrType) != 1 {
		return false
	}
	switch addrType[0] {
	case 'u', 'c', 'v', 's':
		return true
	}
	return false
}

// networkToString converts a NetworkType to its string representation.
func networkToString(network NetworkType) string {
	switch network {
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
		return "main"
	}
}

// GetNetworkFromString parses a network string to NetworkType.
func GetNetworkFromString(s string) NetworkType {
	switch s {
	case "main":
		return NetworkMain
	case "test":
		return NetworkTest
	case "dev":
		return NetworkDev
	case "lab":
		return NetworkLab
	case "staging":
		return NetworkStaging
	default:
		return NetworkMain
	}
}
