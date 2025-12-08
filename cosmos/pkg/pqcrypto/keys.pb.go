// Code generated manually for ML-DSA-87 protobuf compatibility.
// This provides the necessary protobuf interfaces for Cosmos SDK integration.

package pqcrypto

import (
	"github.com/cosmos/cosmos-sdk/codec/types"
	"github.com/cosmos/gogoproto/proto"
)

func init() {
	// Register types with gogoproto global registry
	proto.RegisterType((*PubKey)(nil), "aegisvm.crypto.mldsa87.PubKey")
	proto.RegisterType((*PrivKey)(nil), "aegisvm.crypto.mldsa87.PrivKey")
}

// Type URLs for ML-DSA-87 keys
const (
	PubKeyTypeURL  = "/aegisvm.crypto.mldsa87.PubKey"
	PrivKeyTypeURL = "/aegisvm.crypto.mldsa87.PrivKey"
)

// Ensure the types implement proto.Message
var (
	_ proto.Message = &PubKey{}
	_ proto.Message = &PrivKey{}
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
