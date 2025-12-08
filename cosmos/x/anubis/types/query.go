// Query types for x/anubis

package types

import (
	"context"
	"fmt"

	"google.golang.org/grpc"
)

// Proto interface implementations for query types
func (m *QueryParamsRequest) Reset()         { *m = QueryParamsRequest{} }
func (m *QueryParamsRequest) String() string { return "QueryParamsRequest" }
func (m *QueryParamsRequest) ProtoMessage()  {}

func (m *QueryParamsResponse) Reset()         { *m = QueryParamsResponse{} }
func (m *QueryParamsResponse) String() string { return fmt.Sprintf("%+v", *m) }
func (m *QueryParamsResponse) ProtoMessage()  {}

func (m *QueryPrivateContractRequest) Reset()         { *m = QueryPrivateContractRequest{} }
func (m *QueryPrivateContractRequest) String() string { return m.Address }
func (m *QueryPrivateContractRequest) ProtoMessage()  {}

func (m *QueryPrivateContractResponse) Reset()         { *m = QueryPrivateContractResponse{} }
func (m *QueryPrivateContractResponse) String() string { return fmt.Sprintf("%+v", *m) }
func (m *QueryPrivateContractResponse) ProtoMessage()  {}

func (m *QuerySessionRequest) Reset()         { *m = QuerySessionRequest{} }
func (m *QuerySessionRequest) String() string { return fmt.Sprintf("%x", m.SessionId) }
func (m *QuerySessionRequest) ProtoMessage()  {}

func (m *QuerySessionResponse) Reset()         { *m = QuerySessionResponse{} }
func (m *QuerySessionResponse) String() string { return fmt.Sprintf("%+v", *m) }
func (m *QuerySessionResponse) ProtoMessage()  {}

func (m *QueryEncryptedCallRequest) Reset()         { *m = QueryEncryptedCallRequest{} }
func (m *QueryEncryptedCallRequest) String() string { return fmt.Sprintf("%x", m.CallId) }
func (m *QueryEncryptedCallRequest) ProtoMessage()  {}

func (m *QueryEncryptedCallResponse) Reset()         { *m = QueryEncryptedCallResponse{} }
func (m *QueryEncryptedCallResponse) String() string { return fmt.Sprintf("%+v", *m) }
func (m *QueryEncryptedCallResponse) ProtoMessage()  {}

func (m *QueryZKProofRequest) Reset()         { *m = QueryZKProofRequest{} }
func (m *QueryZKProofRequest) String() string { return fmt.Sprintf("%x", m.ProofHash) }
func (m *QueryZKProofRequest) ProtoMessage()  {}

func (m *QueryZKProofResponse) Reset()         { *m = QueryZKProofResponse{} }
func (m *QueryZKProofResponse) String() string { return fmt.Sprintf("%+v", *m) }
func (m *QueryZKProofResponse) ProtoMessage()  {}

// Query request types
type QueryParamsRequest struct{}

type QueryParamsResponse struct {
	Params Params `json:"params"`
}

type QueryPrivateContractRequest struct {
	Address string `json:"address"`
}

type QueryPrivateContractResponse struct {
	PrivateContract PrivateContract `json:"private_contract"`
}

type QuerySessionRequest struct {
	SessionId []byte `json:"session_id"`
}

type QuerySessionResponse struct {
	Session SessionKey `json:"session"`
}

type QueryEncryptedCallRequest struct {
	CallId []byte `json:"call_id"`
}

type QueryEncryptedCallResponse struct {
	EncryptedCall EncryptedCall `json:"encrypted_call"`
}

type QueryZKProofRequest struct {
	ProofHash []byte `json:"proof_hash"`
}

type QueryZKProofResponse struct {
	Proof ZKStateProof `json:"proof"`
}

// QueryClient is the client API for Anubis query service
type QueryClient interface {
	// Params returns the module parameters
	Params(ctx context.Context, in *QueryParamsRequest, opts ...grpc.CallOption) (*QueryParamsResponse, error)

	// PrivateContract returns private execution config for a contract
	PrivateContract(ctx context.Context, in *QueryPrivateContractRequest, opts ...grpc.CallOption) (*QueryPrivateContractResponse, error)

	// Session returns a session by ID
	Session(ctx context.Context, in *QuerySessionRequest, opts ...grpc.CallOption) (*QuerySessionResponse, error)

	// EncryptedCall returns an encrypted call by ID
	EncryptedCall(ctx context.Context, in *QueryEncryptedCallRequest, opts ...grpc.CallOption) (*QueryEncryptedCallResponse, error)

	// ZKProof returns a ZK proof by hash
	ZKProof(ctx context.Context, in *QueryZKProofRequest, opts ...grpc.CallOption) (*QueryZKProofResponse, error)
}

type queryClient struct {
	cc grpc.ClientConnInterface
}

// NewQueryClient creates a new query client
func NewQueryClient(cc grpc.ClientConnInterface) QueryClient {
	return &queryClient{cc}
}

func (c *queryClient) Params(ctx context.Context, in *QueryParamsRequest, opts ...grpc.CallOption) (*QueryParamsResponse, error) {
	out := new(QueryParamsResponse)
	err := c.cc.Invoke(ctx, "/anubis.Query/Params", in, out, opts...)
	if err != nil {
		return nil, err
	}
	return out, nil
}

func (c *queryClient) PrivateContract(ctx context.Context, in *QueryPrivateContractRequest, opts ...grpc.CallOption) (*QueryPrivateContractResponse, error) {
	out := new(QueryPrivateContractResponse)
	err := c.cc.Invoke(ctx, "/anubis.Query/PrivateContract", in, out, opts...)
	if err != nil {
		return nil, err
	}
	return out, nil
}

func (c *queryClient) Session(ctx context.Context, in *QuerySessionRequest, opts ...grpc.CallOption) (*QuerySessionResponse, error) {
	out := new(QuerySessionResponse)
	err := c.cc.Invoke(ctx, "/anubis.Query/Session", in, out, opts...)
	if err != nil {
		return nil, err
	}
	return out, nil
}

func (c *queryClient) EncryptedCall(ctx context.Context, in *QueryEncryptedCallRequest, opts ...grpc.CallOption) (*QueryEncryptedCallResponse, error) {
	out := new(QueryEncryptedCallResponse)
	err := c.cc.Invoke(ctx, "/anubis.Query/EncryptedCall", in, out, opts...)
	if err != nil {
		return nil, err
	}
	return out, nil
}

func (c *queryClient) ZKProof(ctx context.Context, in *QueryZKProofRequest, opts ...grpc.CallOption) (*QueryZKProofResponse, error) {
	out := new(QueryZKProofResponse)
	err := c.cc.Invoke(ctx, "/anubis.Query/ZKProof", in, out, opts...)
	if err != nil {
		return nil, err
	}
	return out, nil
}
