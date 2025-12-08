package encoding

import (
	"fmt"

	"github.com/cometbft/cometbft/crypto"
	"github.com/cometbft/cometbft/crypto/ed25519"
	"github.com/cometbft/cometbft/crypto/secp256k1"
	"github.com/cometbft/cometbft/crypto/tmhash"
	"github.com/cometbft/cometbft/libs/json"
	pc "github.com/cometbft/cometbft/proto/tendermint/crypto"
)

// ML-DSA-87 constants
const (
	MLDSAPublicKeySize = 2592 // ML-DSA-87 public key size (NIST FIPS 204)
	MLDSAKeyType       = "ml-dsa-87"
)

func init() {
	json.RegisterType((*pc.PublicKey)(nil), "tendermint.crypto.PublicKey")
	json.RegisterType((*pc.PublicKey_Ed25519)(nil), "tendermint.crypto.PublicKey_Ed25519")
	json.RegisterType((*pc.PublicKey_Secp256K1)(nil), "tendermint.crypto.PublicKey_Secp256K1")
}

// PubKeyToProto takes crypto.PubKey and transforms it to a protobuf Pubkey.
// Extended with ML-DSA-87 support for post-quantum security.
func PubKeyToProto(k crypto.PubKey) (pc.PublicKey, error) {
	var kp pc.PublicKey

	// Check for ML-DSA-87 key first (by type string or size)
	if k != nil {
		keyType := k.Type()
		keyBytes := k.Bytes()

		if keyType == MLDSAKeyType || len(keyBytes) == MLDSAPublicKeySize {
			// ML-DSA-87: Pack into ed25519 field (differentiated by size at decode)
			kp = pc.PublicKey{
				Sum: &pc.PublicKey_Ed25519{
					Ed25519: keyBytes,
				},
			}
			return kp, nil
		}
	}

	switch k := k.(type) {
	case ed25519.PubKey:
		kp = pc.PublicKey{
			Sum: &pc.PublicKey_Ed25519{
				Ed25519: k,
			},
		}
	case secp256k1.PubKey:
		kp = pc.PublicKey{
			Sum: &pc.PublicKey_Secp256K1{
				Secp256K1: k,
			},
		}
	default:
		return kp, fmt.Errorf("toproto: key type %v is not supported", k)
	}
	return kp, nil
}

// PubKeyFromProto takes a protobuf Pubkey and transforms it to a crypto.Pubkey.
// Extended with ML-DSA-87 support for post-quantum security.
func PubKeyFromProto(k pc.PublicKey) (crypto.PubKey, error) {
	switch pk := k.Sum.(type) {
	case *pc.PublicKey_Ed25519:
		// Check if this is actually an ML-DSA-87 key by size
		if len(pk.Ed25519) == MLDSAPublicKeySize {
			// Return as mldsaPubKey wrapper for proper type handling
			return mldsaPubKey(pk.Ed25519), nil
		}
		// Standard ed25519
		if len(pk.Ed25519) != ed25519.PubKeySize {
			return nil, fmt.Errorf("invalid size for PubKeyEd25519. Got %d, expected %d",
				len(pk.Ed25519), ed25519.PubKeySize)
		}
		edPK := make(ed25519.PubKey, ed25519.PubKeySize)
		copy(edPK, pk.Ed25519)
		return edPK, nil
	case *pc.PublicKey_Secp256K1:
		if len(pk.Secp256K1) != secp256k1.PubKeySize {
			return nil, fmt.Errorf("invalid size for PubKeySecp256k1. Got %d, expected %d",
				len(pk.Secp256K1), secp256k1.PubKeySize)
		}
		secpPK := make(secp256k1.PubKey, secp256k1.PubKeySize)
		copy(secpPK, pk.Secp256K1)
		return secpPK, nil
	default:
		return nil, fmt.Errorf("fromproto: key type %v is not supported", k)
	}
}

// mldsaPubKey is a minimal crypto.PubKey implementation for ML-DSA-87
// used during protobuf deserialization within CometBFT.
type mldsaPubKey []byte

func (pk mldsaPubKey) Address() crypto.Address {
	// Use tmhash.SumTruncated for CometBFT compatibility (20 bytes)
	// This is SHA256 truncated to 20 bytes, matching ed25519 address derivation
	// Note: AAS-001 addresses use full 32-byte account IDs, but CometBFT
	// validator addresses require 20 bytes internally
	return crypto.Address(tmhash.SumTruncated(pk))
}

func (pk mldsaPubKey) Bytes() []byte {
	return pk
}

func (pk mldsaPubKey) VerifySignature(msg []byte, sig []byte) bool {
	// Placeholder - the actual ML-DSA-87 implementation in pqcrypto handles verification
	// This type is primarily for protobuf transport, not direct use
	return false
}

func (pk mldsaPubKey) Equals(other crypto.PubKey) bool {
	if other == nil {
		return false
	}
	otherBytes := other.Bytes()
	if len(pk) != len(otherBytes) {
		return false
	}
	for i := range pk {
		if pk[i] != otherBytes[i] {
			return false
		}
	}
	return true
}

func (pk mldsaPubKey) Type() string {
	return MLDSAKeyType
}
