// Package pqcrypto provides post-quantum cryptographic keys for AegisVM.
//
// This implements Cosmos SDK crypto.PrivKey and crypto.PubKey interfaces
// using ML-DSA-87 (NIST FIPS 204) for post-quantum security.
package pqcrypto

import (
	"bytes"
	"crypto/subtle"
	"fmt"
	"io"

	"github.com/cometbft/cometbft/crypto"
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
	// Interface assertions for Cosmos SDK compatibility
	_ cryptotypes.PrivKey = &PrivKey{}
	_ cryptotypes.PubKey  = &PubKey{}
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
func (privKey *PrivKey) PubKey() cryptotypes.PubKey {
	pk := privKey.Key.Public()
	return &PubKey{Key: pk}
}

// Type returns the key type string.
func (privKey *PrivKey) Type() string {
	return KeyType
}

// Equals compares two private keys.
func (privKey *PrivKey) Equals(other cryptotypes.LedgerPrivKey) bool {
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

// Address returns the 32-byte account ID derived from the public key.
// Uses domain-separated SHA3-256 hashing per AAS-001 v3.1:
// SHA3-256("aegis-v1-mldsa87-u" || public_key)
func (pubKey *PubKey) Address() crypto.Address {
	accountID, err := DeriveAccountID(pubKey.Key, AddressTypeUser)
	if err != nil {
		// Fallback to direct hash if FFI fails
		hash := sha3.Sum256(pubKey.Key[:])
		return crypto.Address(hash[:AccountIDSize])
	}
	return crypto.Address(accountID[:])
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

// Equals compares two public keys.
func (pubKey *PubKey) Equals(other cryptotypes.PubKey) bool {
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
	registry.RegisterImplementations((*cryptotypes.PubKey)(nil), &PubKey{})
	registry.RegisterImplementations((*cryptotypes.PrivKey)(nil), &PrivKey{})
}
