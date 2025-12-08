// Code generated manually for ML-DSA-87 protobuf compatibility.
// This provides the necessary protobuf interfaces for Cosmos SDK integration.

package pqcrypto

import (
	"github.com/cometbft/cometbft/libs/json"
	cmttypes "github.com/cometbft/cometbft/types"
	"github.com/cosmos/cosmos-sdk/codec/types"
	"github.com/cosmos/gogoproto/proto"
)

func init() {
	// Register types with gogoproto global registry
	// Register both PubKey and SDKPubKey under the same proto name
	// This allows the Cosmos SDK to unmarshal genesis JSON into SDKPubKey
	proto.RegisterType((*PubKey)(nil), "aegisvm.crypto.mldsa87.PubKey")
	proto.RegisterType((*PrivKey)(nil), "aegisvm.crypto.mldsa87.PrivKey")
	proto.RegisterType((*SDKPubKey)(nil), "aegisvm.crypto.mldsa87.SDKPubKey")
	proto.RegisterType((*SDKPrivKey)(nil), "aegisvm.crypto.mldsa87.SDKPrivKey")

	// Register types with CometBFT's JSON registry for priv_validator_key.json parsing
	// The types must be registered as implementations of the crypto.PubKey/PrivKey interfaces
	json.RegisterType((*PubKey)(nil), "aegisvm/PubKeyMLDSA87")
	json.RegisterType((*PrivKey)(nil), "aegisvm/PrivKeyMLDSA87")

	// Register ML-DSA-87 as a known validator public key type in CometBFT
	// This allows it to be used in consensus_params.validator.pub_key_types
	cmttypes.ABCIPubKeyTypesToNames[KeyType] = PubKeyName
}

// Note: CometBFT crypto interfaces use different Equals signatures than Cosmos SDK.
// Our types implement Cosmos SDK interfaces. CometBFT JSON registration works
// because it only requires the type to be registered, not interface implementation.

// Type URLs for ML-DSA-87 keys
const (
	PubKeyTypeURL  = "/aegisvm.crypto.mldsa87.PubKey"
	PrivKeyTypeURL = "/aegisvm.crypto.mldsa87.PrivKey"
)

// Ensure the types implement proto.Message
var (
	_ proto.Message = &PubKey{}
	_ proto.Message = &PrivKey{}
	_ proto.Message = &SDKPubKey{}
	_ proto.Message = &SDKPrivKey{}
)

// XXX_MessageName returns the proto message name for PubKey.
func (*PubKey) XXX_MessageName() string {
	return "aegisvm.crypto.mldsa87.PubKey"
}

// XXX_MessageName returns the proto message name for PrivKey.
func (*PrivKey) XXX_MessageName() string {
	return "aegisvm.crypto.mldsa87.PrivKey"
}

// Marshal implements proto.Message for PubKey.
func (m *PubKey) Marshal() ([]byte, error) {
	return m.Key[:], nil
}

// MarshalTo implements proto.Message for PubKey.
func (m *PubKey) MarshalTo(data []byte) (int, error) {
	copy(data, m.Key[:])
	return len(m.Key), nil
}

// Unmarshal implements proto.Message for PubKey.
func (m *PubKey) Unmarshal(data []byte) error {
	if len(data) != PublicKeySize {
		return ErrInvalidPublicKeySize
	}
	copy(m.Key[:], data)
	return nil
}

// Size implements proto.Message for PubKey.
func (m *PubKey) Size() int {
	return PublicKeySize
}

// Marshal implements proto.Message for PrivKey.
func (m *PrivKey) Marshal() ([]byte, error) {
	return m.Key[:], nil
}

// MarshalTo implements proto.Message for PrivKey.
func (m *PrivKey) MarshalTo(data []byte) (int, error) {
	copy(data, m.Key[:])
	return len(m.Key), nil
}

// Unmarshal implements proto.Message for PrivKey.
func (m *PrivKey) Unmarshal(data []byte) error {
	if len(data) != SecretKeySize {
		return ErrInvalidSecretKeySize
	}
	copy(m.Key[:], data)
	return nil
}

// Size implements proto.Message for PrivKey.
func (m *PrivKey) Size() int {
	return SecretKeySize
}

// RegisterMLDSATypes registers ML-DSA-87 types with the interface registry.
// This is called from RegisterInterfaces in keys.go.
func RegisterMLDSATypes(registry types.InterfaceRegistry) {
	registry.RegisterImplementations(
		(*types.UnpackInterfacesMessage)(nil),
	)
}

// Errors
var (
	ErrInvalidPublicKeySize = &invalidKeySizeError{"public key", PublicKeySize}
	ErrInvalidSecretKeySize = &invalidKeySizeError{"secret key", SecretKeySize}
)

type invalidKeySizeError struct {
	keyType      string
	expectedSize int
}

func (e *invalidKeySizeError) Error() string {
	return "invalid " + e.keyType + " size"
}

// ============================================================================
// SDKPubKey proto.Message implementation
// ============================================================================

// XXX_MessageName returns the proto message name for SDKPubKey.
func (*SDKPubKey) XXX_MessageName() string {
	return "aegisvm.crypto.mldsa87.PubKey"
}

// Marshal implements proto.Message for SDKPubKey.
func (m *SDKPubKey) Marshal() ([]byte, error) {
	if m == nil || len(m.Key) == 0 {
		return nil, nil
	}
	return m.Key, nil
}

// MarshalTo implements proto.Message for SDKPubKey.
func (m *SDKPubKey) MarshalTo(data []byte) (int, error) {
	if m == nil || len(m.Key) == 0 {
		return 0, nil
	}
	copy(data, m.Key)
	return len(m.Key), nil
}

// Unmarshal implements proto.Message for SDKPubKey.
func (m *SDKPubKey) Unmarshal(data []byte) error {
	if len(data) != PublicKeySize {
		return ErrInvalidPublicKeySize
	}
	m.Key = make([]byte, PublicKeySize)
	copy(m.Key, data)
	return nil
}

// Size implements proto.Message for SDKPubKey.
func (m *SDKPubKey) Size() int {
	if m == nil || len(m.Key) == 0 {
		return 0
	}
	return len(m.Key)
}

// ============================================================================
// SDKPrivKey proto.Message implementation
// ============================================================================

// XXX_MessageName returns the proto message name for SDKPrivKey.
func (*SDKPrivKey) XXX_MessageName() string {
	return "aegisvm.crypto.mldsa87.PrivKey"
}

// Marshal implements proto.Message for SDKPrivKey.
func (m *SDKPrivKey) Marshal() ([]byte, error) {
	if m.PrivKey == nil {
		return nil, nil
	}
	return m.PrivKey.Key[:], nil
}

// MarshalTo implements proto.Message for SDKPrivKey.
func (m *SDKPrivKey) MarshalTo(data []byte) (int, error) {
	if m.PrivKey == nil {
		return 0, nil
	}
	copy(data, m.PrivKey.Key[:])
	return len(m.PrivKey.Key), nil
}

// Unmarshal implements proto.Message for SDKPrivKey.
func (m *SDKPrivKey) Unmarshal(data []byte) error {
	if len(data) != SecretKeySize {
		return ErrInvalidSecretKeySize
	}
	if m.PrivKey == nil {
		m.PrivKey = &PrivKey{}
	}
	copy(m.PrivKey.Key[:], data)
	return nil
}

// Size implements proto.Message for SDKPrivKey.
func (m *SDKPrivKey) Size() int {
	if m.PrivKey == nil {
		return 0
	}
	return SecretKeySize
}
