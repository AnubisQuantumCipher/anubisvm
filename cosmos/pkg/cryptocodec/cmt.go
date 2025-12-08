// Package cryptocodec provides cryptographic codec functions with ML-DSA-87 support.
// This package wraps the Cosmos SDK's crypto codec to add post-quantum key support.
package cryptocodec

import (
	cmtcrypto "github.com/cometbft/cometbft/crypto"
	"github.com/cometbft/cometbft/crypto/encoding"
	cmtprotocrypto "github.com/cometbft/cometbft/proto/tendermint/crypto"

	"cosmossdk.io/errors"

	"github.com/aegisvm/cosmos/pkg/pqcrypto"
	"github.com/cosmos/cosmos-sdk/crypto/keys/ed25519"
	"github.com/cosmos/cosmos-sdk/crypto/keys/secp256k1"
	cryptotypes "github.com/cosmos/cosmos-sdk/crypto/types"
	sdkerrors "github.com/cosmos/cosmos-sdk/types/errors"
)

// FromCmtProtoPublicKey converts a CMT's cmtprotocrypto.PublicKey into our own PubKey.
// Extended with ML-DSA-87 support.
func FromCmtProtoPublicKey(protoPk cmtprotocrypto.PublicKey) (cryptotypes.PubKey, error) {
	switch protoPk := protoPk.Sum.(type) {
	case *cmtprotocrypto.PublicKey_Ed25519:
		// Check if this is actually an ML-DSA-87 key by size
		if len(protoPk.Ed25519) == pqcrypto.PublicKeySize {
			return pqcrypto.NewSDKPubKey(protoPk.Ed25519)
		}
		return &ed25519.PubKey{
			Key: protoPk.Ed25519,
		}, nil
	case *cmtprotocrypto.PublicKey_Secp256K1:
		return &secp256k1.PubKey{
			Key: protoPk.Secp256K1,
		}, nil
	default:
		return nil, errors.Wrapf(sdkerrors.ErrInvalidType, "cannot convert %v from Tendermint public key", protoPk)
	}
}

// ToCmtProtoPublicKey converts our own PubKey to Cmt's cmtprotocrypto.PublicKey.
// Extended with ML-DSA-87 support.
func ToCmtProtoPublicKey(pk cryptotypes.PubKey) (cmtprotocrypto.PublicKey, error) {
	// Handle ML-DSA-87 keys first (pack into Ed25519 field, differentiated by size)
	switch pk := pk.(type) {
	case *pqcrypto.SDKPubKey:
		// ML-DSA-87: Pack into ed25519 field (CometBFT differentiates by size)
		return cmtprotocrypto.PublicKey{
			Sum: &cmtprotocrypto.PublicKey_Ed25519{
				Ed25519: pk.Key,
			},
		}, nil
	case *pqcrypto.PubKey:
		// CometBFT ML-DSA-87 key
		return cmtprotocrypto.PublicKey{
			Sum: &cmtprotocrypto.PublicKey_Ed25519{
				Ed25519: pk.Key[:],
			},
		}, nil
	case *ed25519.PubKey:
		return cmtprotocrypto.PublicKey{
			Sum: &cmtprotocrypto.PublicKey_Ed25519{
				Ed25519: pk.Key,
			},
		}, nil
	case *secp256k1.PubKey:
		return cmtprotocrypto.PublicKey{
			Sum: &cmtprotocrypto.PublicKey_Secp256K1{
				Secp256K1: pk.Key,
			},
		}, nil
	default:
		// Check by type string and key size as fallback
		if pk != nil && pk.Type() == pqcrypto.KeyType {
			return cmtprotocrypto.PublicKey{
				Sum: &cmtprotocrypto.PublicKey_Ed25519{
					Ed25519: pk.Bytes(),
				},
			}, nil
		}
		return cmtprotocrypto.PublicKey{}, errors.Wrapf(sdkerrors.ErrInvalidType, "cannot convert %v to Tendermint public key", pk)
	}
}

// FromCmtPubKeyInterface converts CMT's cmtcrypto.PubKey to our own PubKey.
func FromCmtPubKeyInterface(tmPk cmtcrypto.PubKey) (cryptotypes.PubKey, error) {
	tmProtoPk, err := encoding.PubKeyToProto(tmPk)
	if err != nil {
		return nil, err
	}

	return FromCmtProtoPublicKey(tmProtoPk)
}

// ToCmtPubKeyInterface converts our own PubKey to CMT's cmtcrypto.PubKey.
func ToCmtPubKeyInterface(pk cryptotypes.PubKey) (cmtcrypto.PubKey, error) {
	tmProtoPk, err := ToCmtProtoPublicKey(pk)
	if err != nil {
		return nil, err
	}

	return encoding.PubKeyFromProto(tmProtoPk)
}

// ----------------------
// Deprecated function aliases for compatibility

// Deprecated: use FromCmtProtoPublicKey instead.
func FromTmProtoPublicKey(protoPk cmtprotocrypto.PublicKey) (cryptotypes.PubKey, error) {
	return FromCmtProtoPublicKey(protoPk)
}

// Deprecated: use ToCmtProtoPublicKey instead.
func ToTmProtoPublicKey(pk cryptotypes.PubKey) (cmtprotocrypto.PublicKey, error) {
	return ToCmtProtoPublicKey(pk)
}

// Deprecated: use FromCmtPubKeyInterface instead.
func FromTmPubKeyInterface(tmPk cmtcrypto.PubKey) (cryptotypes.PubKey, error) {
	return FromCmtPubKeyInterface(tmPk)
}

// Deprecated: use ToCmtPubKeyInterface instead.
func ToTmPubKeyInterface(pk cryptotypes.PubKey) (cmtcrypto.PubKey, error) {
	return ToCmtPubKeyInterface(pk)
}
