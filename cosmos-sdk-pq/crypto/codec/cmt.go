package codec

import (
	cmtcrypto "github.com/cometbft/cometbft/crypto"
	"github.com/cometbft/cometbft/crypto/encoding"
	cmtprotocrypto "github.com/cometbft/cometbft/proto/tendermint/crypto"

	"cosmossdk.io/errors"

	cryptotypes "github.com/cosmos/cosmos-sdk/crypto/types"
	sdkerrors "github.com/cosmos/cosmos-sdk/types/errors"
)

// ML-DSA-87 constants - POST-QUANTUM ONLY chain
const (
	MLDSAPublicKeySize = 2592 // ML-DSA-87 public key size (NIST FIPS 204 Level 5)
	MLDSAKeyType       = "ml-dsa-87"
)

// MLDSAPubKey is the SDK wrapper for ML-DSA-87 public keys
// This is the ONLY key type supported on this post-quantum chain
type MLDSAPubKey struct {
	Key []byte
}

func (pk *MLDSAPubKey) Address() cryptotypes.Address {
	if len(pk.Key) >= 32 {
		return pk.Key[:32]
	}
	return nil
}

func (pk *MLDSAPubKey) Bytes() []byte {
	return pk.Key
}

func (pk *MLDSAPubKey) VerifySignature(msg []byte, sig []byte) bool {
	// Actual verification happens in pqcrypto package via FFI to SPARK/Ada
	return false
}

func (pk *MLDSAPubKey) Equals(other cryptotypes.PubKey) bool {
	if other == nil {
		return false
	}
	otherBytes := other.Bytes()
	if len(pk.Key) != len(otherBytes) {
		return false
	}
	for i := range pk.Key {
		if pk.Key[i] != otherBytes[i] {
			return false
		}
	}
	return true
}

func (pk *MLDSAPubKey) Type() string {
	return MLDSAKeyType
}

func (pk *MLDSAPubKey) Reset()         { pk.Key = nil }
func (pk *MLDSAPubKey) String() string { return "MLDSAPubKey" }
func (pk *MLDSAPubKey) ProtoMessage()  {}

// FromCmtProtoPublicKey converts CometBFT's protobuf PublicKey to SDK PubKey.
// POST-QUANTUM ONLY: Only ML-DSA-87 keys are accepted.
func FromCmtProtoPublicKey(protoPk cmtprotocrypto.PublicKey) (cryptotypes.PubKey, error) {
	switch pk := protoPk.Sum.(type) {
	case *cmtprotocrypto.PublicKey_Ed25519:
		// ML-DSA-87 keys are packed into Ed25519 field, differentiated by size
		if len(pk.Ed25519) == MLDSAPublicKeySize {
			return &MLDSAPubKey{Key: pk.Ed25519}, nil
		}
		return nil, errors.Wrapf(sdkerrors.ErrInvalidType, "invalid key size %d, expected ML-DSA-87 (%d bytes)", len(pk.Ed25519), MLDSAPublicKeySize)
	default:
		return nil, errors.Wrapf(sdkerrors.ErrInvalidType, "unsupported key type %T, only ML-DSA-87 supported", protoPk.Sum)
	}
}

// ToCmtProtoPublicKey converts SDK PubKey to CometBFT's protobuf PublicKey.
// POST-QUANTUM ONLY: Only ML-DSA-87 keys are accepted.
func ToCmtProtoPublicKey(pk cryptotypes.PubKey) (cmtprotocrypto.PublicKey, error) {
	if pk == nil {
		return cmtprotocrypto.PublicKey{}, errors.Wrap(sdkerrors.ErrInvalidType, "public key is nil")
	}

	keyBytes := pk.Bytes()

	// Accept any key that is ML-DSA-87 sized or has the correct type
	if pk.Type() == MLDSAKeyType || len(keyBytes) == MLDSAPublicKeySize {
		return cmtprotocrypto.PublicKey{
			Sum: &cmtprotocrypto.PublicKey_Ed25519{
				Ed25519: keyBytes,
			},
		}, nil
	}

	return cmtprotocrypto.PublicKey{}, errors.Wrapf(sdkerrors.ErrInvalidType, "unsupported key type %s, only ML-DSA-87 supported", pk.Type())
}

// FromCmtPubKeyInterface converts CMT's cmtcrypto.PubKey to SDK PubKey.
func FromCmtPubKeyInterface(tmPk cmtcrypto.PubKey) (cryptotypes.PubKey, error) {
	tmProtoPk, err := encoding.PubKeyToProto(tmPk)
	if err != nil {
		return nil, err
	}

	return FromCmtProtoPublicKey(tmProtoPk)
}

// ToCmtPubKeyInterface converts SDK PubKey to CMT's cmtcrypto.PubKey.
func ToCmtPubKeyInterface(pk cryptotypes.PubKey) (cmtcrypto.PubKey, error) {
	tmProtoPk, err := ToCmtProtoPublicKey(pk)
	if err != nil {
		return nil, err
	}

	return encoding.PubKeyFromProto(tmProtoPk)
}

// ----------------------
// Deprecated compatibility aliases

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
