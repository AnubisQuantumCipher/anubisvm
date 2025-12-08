// Package encoding provides protobuf encoding support for ML-DSA-87 keys.
//
// This package extends CometBFT's crypto/encoding package to support
// ML-DSA-87 post-quantum signatures alongside the standard ed25519 and secp256k1.
package encoding

import (
	"fmt"

	"github.com/cometbft/cometbft/crypto"
	"github.com/cometbft/cometbft/crypto/ed25519"
	"github.com/cometbft/cometbft/crypto/secp256k1"
	"github.com/cometbft/cometbft/libs/json"
	pc "github.com/cometbft/cometbft/proto/tendermint/crypto"

	"github.com/aegisvm/cosmos/pkg/pqcrypto"
)

func init() {
	json.RegisterType((*pc.PublicKey)(nil), "tendermint.crypto.PublicKey")
	json.RegisterType((*pc.PublicKey_Ed25519)(nil), "tendermint.crypto.PublicKey_Ed25519")
	json.RegisterType((*pc.PublicKey_Secp256K1)(nil), "tendermint.crypto.PublicKey_Secp256K1")
}

// PubKeyToProto takes crypto.PubKey and transforms it to a protobuf Pubkey.
// Extends CometBFT's version with ML-DSA-87 support.
func PubKeyToProto(k crypto.PubKey) (pc.PublicKey, error) {
	var kp pc.PublicKey
	switch pk := k.(type) {
	case ed25519.PubKey:
		kp = pc.PublicKey{
			Sum: &pc.PublicKey_Ed25519{
				Ed25519: pk,
			},
		}
	case secp256k1.PubKey:
		kp = pc.PublicKey{
			Sum: &pc.PublicKey_Secp256K1{
				Secp256K1: pk,
			},
		}
	case *pqcrypto.PubKey:
		// ML-DSA-87: Use ed25519 field with custom encoding
		// The type is determined by key size (2592 bytes for ML-DSA-87)
		kp = pc.PublicKey{
			Sum: &pc.PublicKey_Ed25519{
				Ed25519: pk.Key[:],
			},
		}
	default:
		// Check by type string for indirect types
		if k != nil && k.Type() == pqcrypto.KeyType {
			kp = pc.PublicKey{
				Sum: &pc.PublicKey_Ed25519{
					Ed25519: k.Bytes(),
				},
			}
		} else {
			return kp, fmt.Errorf("toproto: key type %v is not supported", k)
		}
	}
	return kp, nil
}

// PubKeyFromProto takes a protobuf Pubkey and transforms it to a crypto.Pubkey.
// Extends CometBFT's version with ML-DSA-87 support.
func PubKeyFromProto(k pc.PublicKey) (crypto.PubKey, error) {
	switch pk := k.Sum.(type) {
	case *pc.PublicKey_Ed25519:
		// Check if this is actually an ML-DSA-87 key by size
		if len(pk.Ed25519) == pqcrypto.PublicKeySize {
			// This is an ML-DSA-87 key
			mldsaPK, err := pqcrypto.NewPubKey(pk.Ed25519)
			if err != nil {
				return nil, err
			}
			return mldsaPK, nil
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
