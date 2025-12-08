// Package pqcrypto provides post-quantum cryptographic keys for AegisVM.
//
// This implements Cosmos SDK crypto.PrivKey and crypto.PubKey interfaces
// using ML-DSA-87 (NIST FIPS 204) for post-quantum security.
package pqcrypto

import (
	"bytes"
	"crypto/sha256"
	"crypto/subtle"
	"encoding/base64"
	"encoding/json"
	"fmt"
	"io"

	cmtcrypto "github.com/cometbft/cometbft/crypto"
	"github.com/cosmos/cosmos-sdk/codec"
	codectypes "github.com/cosmos/cosmos-sdk/codec/types"
	cryptotypes "github.com/cosmos/cosmos-sdk/crypto/types"
	"golang.org/x/crypto/sha3"
)

const (
	// KeyType is the string constant for ML-DSA-87 keys
	KeyType = "ml-dsa-87"

	// PubKeyName is the amino encoding name for ML-DSA-87 public keys
	PubKeyName = "aegisvm/PubKeyMLDSA87"

	// PrivKeyName is the amino encoding name for ML-DSA-87 private keys
	PrivKeyName = "aegisvm/PrivKeyMLDSA87"

	// AddressSizeBytes is the size of an address in bytes (32 bytes = 256 bits)
	// AAS-001 v3.1 uses full 256-bit account IDs for NIST Level 5 security
	AddressSizeBytes = 32
)

var (
	// Interface assertions for CometBFT compatibility (required for FilePV)
	_ cmtcrypto.PrivKey = &PrivKey{}
	_ cmtcrypto.PubKey  = &PubKey{}
)

// PrivKey implements crypto.PrivKey for ML-DSA-87.
type PrivKey struct {
	Key SecretKey
}

// NewPrivKey creates a new ML-DSA-87 private key from bytes.
func NewPrivKey(bz []byte) (*PrivKey, error) {
	if len(bz) != SecretKeySize {
		return nil, fmt.Errorf("invalid ML-DSA-87 secret key size: got %d, expected %d", len(bz), SecretKeySize)
	}
	var sk SecretKey
	copy(sk[:], bz)
	return &PrivKey{Key: sk}, nil
}

// GenPrivKey generates a new ML-DSA-87 private key using the provided reader.
func GenPrivKey() (*PrivKey, error) {
	return GenPrivKeyFromReader(nil)
}

// GenPrivKeyFromReader generates a new ML-DSA-87 private key from the reader.
func GenPrivKeyFromReader(rand io.Reader) (*PrivKey, error) {
	_, sk, err := GenerateKey(rand)
	if err != nil {
		return nil, err
	}
	return &PrivKey{Key: sk}, nil
}

// Bytes returns the byte representation of the private key.
func (privKey *PrivKey) Bytes() []byte {
	return privKey.Key[:]
}

// Sign signs the message using ML-DSA-87.
func (privKey *PrivKey) Sign(msg []byte) ([]byte, error) {
	return Sign(privKey.Key, msg)
}

// PubKey extracts the public key from the private key.
func (privKey *PrivKey) PubKey() cmtcrypto.PubKey {
	pk := privKey.Key.Public()
	return &PubKey{Key: pk}
}

// Type returns the key type string.
func (privKey *PrivKey) Type() string {
	return KeyType
}

// Equals compares two private keys (CometBFT crypto.PrivKey interface).
func (privKey *PrivKey) Equals(other cmtcrypto.PrivKey) bool {
	if otherMLDSA, ok := other.(*PrivKey); ok {
		return subtle.ConstantTimeCompare(privKey.Key[:], otherMLDSA.Key[:]) == 1
	}
	return false
}

// Reset implements proto.Message
func (privKey *PrivKey) Reset() {
	*privKey = PrivKey{}
}

// String implements proto.Message
func (privKey *PrivKey) String() string {
	return fmt.Sprintf("PrivKeyMLDSA87{%X...}", privKey.Key[:8])
}

// ProtoMessage implements proto.Message
func (privKey *PrivKey) ProtoMessage() {}

// MarshalJSON implements json.Marshaler for PrivKey.
// Returns base64-encoded key bytes for CometBFT JSON compatibility.
func (privKey *PrivKey) MarshalJSON() ([]byte, error) {
	return json.Marshal(base64.StdEncoding.EncodeToString(privKey.Key[:]))
}

// UnmarshalJSON implements json.Unmarshaler for PrivKey.
// Expects base64-encoded key bytes.
func (privKey *PrivKey) UnmarshalJSON(data []byte) error {
	var s string
	if err := json.Unmarshal(data, &s); err != nil {
		return err
	}
	bz, err := base64.StdEncoding.DecodeString(s)
	if err != nil {
		return err
	}
	if len(bz) != SecretKeySize {
		return fmt.Errorf("invalid ML-DSA-87 secret key size: got %d, expected %d", len(bz), SecretKeySize)
	}
	copy(privKey.Key[:], bz)
	return nil
}

// PubKey implements crypto.PubKey for ML-DSA-87.
type PubKey struct {
	Key PublicKey
}

// NewPubKey creates a new ML-DSA-87 public key from bytes.
func NewPubKey(bz []byte) (*PubKey, error) {
	if len(bz) != PublicKeySize {
		return nil, fmt.Errorf("invalid ML-DSA-87 public key size: got %d, expected %d", len(bz), PublicKeySize)
	}
	var pk PublicKey
	copy(pk[:], bz)
	return &PubKey{Key: pk}, nil
}

// Address returns a 20-byte address for CometBFT compatibility.
// This is SHA256(pubkey) truncated to 20 bytes, matching ed25519 address derivation.
// Note: For full 32-byte AAS-001 account IDs, use AccountID() instead.
func (pubKey *PubKey) Address() cmtcrypto.Address {
	// Use SHA256 and truncate to 20 bytes for CometBFT (matches tmhash.SumTruncated)
	hash := sha256.Sum256(pubKey.Key[:])
	return cmtcrypto.Address(hash[:20])
}

// AccountID returns the full 32-byte AAS-001 v3.1 account ID.
// Uses domain-separated SHA3-256 hashing:
// SHA3-256("aegis-v1-mldsa87-u" || public_key)
func (pubKey *PubKey) AccountID() AccountID {
	accountID, err := DeriveAccountID(pubKey.Key, AddressTypeUser)
	if err != nil {
		// Fallback to direct hash if FFI fails
		hash := sha3.Sum256(pubKey.Key[:])
		var id AccountID
		copy(id[:], hash[:])
		return id
	}
	return accountID
}

// CanonicalAddress returns the full AAS-001 v3.1 canonical address string.
// Format: mldsa87:main:u:chunked_payload-checksum
// Example: mldsa87:main:u:dztd939y-b16ybkqa-12a55nb1-zyb1f50k-nn08yc11-fskf8sfs-0vsg-bg0bc
func (pubKey *PubKey) CanonicalAddress() string {
	addr, err := FormatUserAddress(pubKey.Key)
	if err != nil {
		return ""
	}
	return addr
}

// CanonicalAddressWithNetwork returns the canonical address for a specific network.
func (pubKey *PubKey) CanonicalAddressWithNetwork(network NetworkType) string {
	addr, err := FormatAddress(pubKey.Key, network, AddressTypeUser)
	if err != nil {
		return ""
	}
	return addr
}

// Bytes returns the byte representation of the public key.
func (pubKey *PubKey) Bytes() []byte {
	return pubKey.Key[:]
}

// VerifySignature verifies an ML-DSA-87 signature.
func (pubKey *PubKey) VerifySignature(msg []byte, sig []byte) bool {
	return Verify(pubKey.Key, msg, sig)
}

// Type returns the key type string.
func (pubKey *PubKey) Type() string {
	return KeyType
}

// Equals compares two public keys (CometBFT crypto.PubKey interface).
func (pubKey *PubKey) Equals(other cmtcrypto.PubKey) bool {
	if otherMLDSA, ok := other.(*PubKey); ok {
		return bytes.Equal(pubKey.Key[:], otherMLDSA.Key[:])
	}
	return false
}

// Reset implements proto.Message
func (pubKey *PubKey) Reset() {
	*pubKey = PubKey{}
}

// String returns a string representation of the public key.
func (pubKey *PubKey) String() string {
	return fmt.Sprintf("PubKeyMLDSA87{%X...}", pubKey.Key[:8])
}

// ProtoMessage implements proto.Message
func (pubKey *PubKey) ProtoMessage() {}

// MarshalJSON implements json.Marshaler for PubKey.
// Returns base64-encoded key bytes for CometBFT JSON compatibility.
func (pubKey *PubKey) MarshalJSON() ([]byte, error) {
	return json.Marshal(base64.StdEncoding.EncodeToString(pubKey.Key[:]))
}

// UnmarshalJSON implements json.Unmarshaler for PubKey.
// Expects base64-encoded key bytes.
func (pubKey *PubKey) UnmarshalJSON(data []byte) error {
	var s string
	if err := json.Unmarshal(data, &s); err != nil {
		return err
	}
	bz, err := base64.StdEncoding.DecodeString(s)
	if err != nil {
		return err
	}
	if len(bz) != PublicKeySize {
		return fmt.Errorf("invalid ML-DSA-87 public key size: got %d, expected %d", len(bz), PublicKeySize)
	}
	copy(pubKey.Key[:], bz)
	return nil
}

// Hash uses SHA3-256 for hashing (replacing SHA256).
func Hash(data []byte) []byte {
	h := sha3.Sum256(data)
	return h[:]
}

// Checksum returns first 4 bytes of SHA3-256 hash.
func Checksum(data []byte) []byte {
	h := sha3.Sum256(data)
	return h[:4]
}

// KeyPair holds both public and private keys.
type KeyPair struct {
	PrivKey *PrivKey
	PubKey  *PubKey
}

// GenerateKeyPair generates a new ML-DSA-87 keypair.
func GenerateKeyPair() (*KeyPair, error) {
	privKey, err := GenPrivKey()
	if err != nil {
		return nil, err
	}
	pubKey := privKey.PubKey().(*PubKey)
	return &KeyPair{
		PrivKey: privKey,
		PubKey:  pubKey,
	}, nil
}

// RegisterLegacyAminoCodec registers the ML-DSA-87 types with the legacy amino codec.
func RegisterLegacyAminoCodec(cdc *codec.LegacyAmino) {
	cdc.RegisterConcrete(&PubKey{}, PubKeyName, nil)
	cdc.RegisterConcrete(&PrivKey{}, PrivKeyName, nil)
}

// RegisterInterfaces registers the ML-DSA-87 types with the interface registry.
func RegisterInterfaces(registry codectypes.InterfaceRegistry) {
	registry.RegisterImplementations((*cryptotypes.PubKey)(nil), &SDKPubKey{})
	registry.RegisterImplementations((*cryptotypes.PrivKey)(nil), &SDKPrivKey{})
}

// ============================================================================
// Cosmos SDK Interface Wrappers
// ============================================================================
// These wrapper types implement Cosmos SDK's cryptotypes interfaces while
// delegating to our CometBFT-compatible core types. This is necessary because
// CometBFT and Cosmos SDK have incompatible Equals() method signatures.

// SDKPrivKey wraps PrivKey to implement cryptotypes.PrivKey
type SDKPrivKey struct {
	*PrivKey
}

// sdkPrivKeyJSON is the JSON structure for SDKPrivKey in protobuf Any format
type sdkPrivKeyJSON struct {
	Key string `json:"key"`
}

// MarshalJSON implements json.Marshaler for SDKPrivKey.
func (sk *SDKPrivKey) MarshalJSON() ([]byte, error) {
	if sk.PrivKey == nil {
		return []byte("null"), nil
	}
	return json.Marshal(sdkPrivKeyJSON{
		Key: base64.StdEncoding.EncodeToString(sk.PrivKey.Key[:]),
	})
}

// UnmarshalJSON implements json.Unmarshaler for SDKPrivKey.
func (sk *SDKPrivKey) UnmarshalJSON(data []byte) error {
	var j sdkPrivKeyJSON
	if err := json.Unmarshal(data, &j); err != nil {
		return err
	}
	bz, err := base64.StdEncoding.DecodeString(j.Key)
	if err != nil {
		return err
	}
	if len(bz) != SecretKeySize {
		return fmt.Errorf("invalid ML-DSA-87 secret key size: got %d, expected %d", len(bz), SecretKeySize)
	}
	if sk.PrivKey == nil {
		sk.PrivKey = &PrivKey{}
	}
	copy(sk.PrivKey.Key[:], bz)
	return nil
}

// Ensure SDKPrivKey implements cryptotypes.PrivKey
var _ cryptotypes.PrivKey = &SDKPrivKey{}

// Bytes returns the byte representation of the private key.
func (sk *SDKPrivKey) Bytes() []byte {
	return sk.PrivKey.Bytes()
}

// Sign signs the message using ML-DSA-87.
func (sk *SDKPrivKey) Sign(msg []byte) ([]byte, error) {
	return sk.PrivKey.Sign(msg)
}

// PubKey extracts the public key from the private key.
func (sk *SDKPrivKey) PubKey() cryptotypes.PubKey {
	pk := sk.PrivKey.Key.Public()
	return &SDKPubKey{Key: pk[:]}
}

// Type returns the key type string.
func (sk *SDKPrivKey) Type() string {
	return sk.PrivKey.Type()
}

// Equals compares two private keys (Cosmos SDK cryptotypes.LedgerPrivKey interface).
func (sk *SDKPrivKey) Equals(other cryptotypes.LedgerPrivKey) bool {
	if otherSDK, ok := other.(*SDKPrivKey); ok {
		return subtle.ConstantTimeCompare(sk.PrivKey.Key[:], otherSDK.PrivKey.Key[:]) == 1
	}
	// Compare by bytes for any other type
	if other != nil && other.Type() == KeyType {
		return subtle.ConstantTimeCompare(sk.Bytes(), other.Bytes()) == 1
	}
	return false
}

// Reset implements proto.Message
func (sk *SDKPrivKey) Reset() {
	if sk.PrivKey != nil {
		sk.PrivKey.Reset()
	}
}

// String implements proto.Message
func (sk *SDKPrivKey) String() string {
	if sk.PrivKey != nil {
		return sk.PrivKey.String()
	}
	return "SDKPrivKeyMLDSA87{nil}"
}

// ProtoMessage implements proto.Message
func (sk *SDKPrivKey) ProtoMessage() {}

// SDKPubKey wraps PubKey to implement cryptotypes.PubKey
// The Key field is exported with JSON tag to match protobuf field name
type SDKPubKey struct {
	Key []byte `json:"key,omitempty" protobuf:"bytes,1,opt,name=key"`
}

// getPubKey returns the embedded PubKey for internal use
func (pk *SDKPubKey) getPubKey() *PubKey {
	if pk == nil || len(pk.Key) == 0 {
		return nil
	}
	p := &PubKey{}
	if len(pk.Key) == PublicKeySize {
		copy(p.Key[:], pk.Key)
	}
	return p
}

// Ensure SDKPubKey implements cryptotypes.PubKey
var _ cryptotypes.PubKey = &SDKPubKey{}

// Address returns the address derived from the public key.
func (pk *SDKPubKey) Address() cmtcrypto.Address {
	if p := pk.getPubKey(); p != nil {
		return p.Address()
	}
	return nil
}

// Bytes returns the byte representation of the public key.
func (pk *SDKPubKey) Bytes() []byte {
	if pk == nil {
		return nil
	}
	return pk.Key
}

// VerifySignature verifies an ML-DSA-87 signature.
func (pk *SDKPubKey) VerifySignature(msg []byte, sig []byte) bool {
	if p := pk.getPubKey(); p != nil {
		return p.VerifySignature(msg, sig)
	}
	return false
}

// Type returns the key type string.
func (pk *SDKPubKey) Type() string {
	return KeyType
}

// Equals compares two public keys (Cosmos SDK cryptotypes.PubKey interface).
func (pk *SDKPubKey) Equals(other cryptotypes.PubKey) bool {
	if otherSDK, ok := other.(*SDKPubKey); ok {
		return bytes.Equal(pk.Key, otherSDK.Key)
	}
	// Compare by bytes for any other type
	if other != nil && other.Type() == KeyType {
		return bytes.Equal(pk.Bytes(), other.Bytes())
	}
	return false
}

// Reset implements proto.Message
func (pk *SDKPubKey) Reset() {
	if pk != nil {
		pk.Key = nil
	}
}

// String implements proto.Message
func (pk *SDKPubKey) String() string {
	if pk != nil && len(pk.Key) >= 8 {
		return fmt.Sprintf("SDKPubKeyMLDSA87{%X...}", pk.Key[:8])
	}
	return "SDKPubKeyMLDSA87{nil}"
}

// ProtoMessage implements proto.Message
func (pk *SDKPubKey) ProtoMessage() {}

// WrapPrivKey wraps a CometBFT-compatible PrivKey for Cosmos SDK use.
func WrapPrivKey(pk *PrivKey) *SDKPrivKey {
	return &SDKPrivKey{pk}
}

// WrapPubKey wraps a CometBFT-compatible PubKey for Cosmos SDK use.
func WrapPubKey(pk *PubKey) *SDKPubKey {
	if pk == nil {
		return &SDKPubKey{}
	}
	return &SDKPubKey{Key: pk.Key[:]}
}

// NewSDKPubKey creates a new SDKPubKey from raw bytes.
func NewSDKPubKey(bz []byte) (*SDKPubKey, error) {
	if len(bz) != PublicKeySize {
		return nil, fmt.Errorf("invalid ML-DSA-87 public key size: got %d, expected %d", len(bz), PublicKeySize)
	}
	key := make([]byte, PublicKeySize)
	copy(key, bz)
	return &SDKPubKey{Key: key}, nil
}
