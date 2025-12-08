// Query client for x/aegisvm
// This provides the gRPC query client interface

package types

import (
	"context"

	"google.golang.org/grpc"
)

// QueryClient is the client API for AegisVM query service
type QueryClient interface {
	// Params returns the module parameters
	Params(ctx context.Context, in *QueryParamsRequest, opts ...grpc.CallOption) (*QueryParamsResponse, error)

	// Contract returns a contract by address
	Contract(ctx context.Context, in *QueryContractRequest, opts ...grpc.CallOption) (*QueryContractResponse, error)

	// ContractState returns the state of a contract
	ContractState(ctx context.Context, in *QueryContractStateRequest, opts ...grpc.CallOption) (*QueryContractStateResponse, error)

	// Bytecode returns bytecode by code hash
	Bytecode(ctx context.Context, in *QueryBytecodeRequest, opts ...grpc.CallOption) (*QueryBytecodeResponse, error)

	// IsCodeApproved checks if a code hash is approved
	IsCodeApproved(ctx context.Context, in *QueryIsCodeApprovedRequest, opts ...grpc.CallOption) (*QueryIsCodeApprovedResponse, error)

	// Contracts returns all contracts
	Contracts(ctx context.Context, in *QueryContractsRequest, opts ...grpc.CallOption) (*QueryContractsResponse, error)

	// SimulateExecution simulates contract execution
	SimulateExecution(ctx context.Context, in *QuerySimulateExecutionRequest, opts ...grpc.CallOption) (*QuerySimulateExecutionResponse, error)
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
	err := c.cc.Invoke(ctx, "/aegisvm.Query/Params", in, out, opts...)
	if err != nil {
		return nil, err
	}
	return out, nil
}

func (c *queryClient) Contract(ctx context.Context, in *QueryContractRequest, opts ...grpc.CallOption) (*QueryContractResponse, error) {
	out := new(QueryContractResponse)
	err := c.cc.Invoke(ctx, "/aegisvm.Query/Contract", in, out, opts...)
	if err != nil {
		return nil, err
	}
	return out, nil
}

func (c *queryClient) ContractState(ctx context.Context, in *QueryContractStateRequest, opts ...grpc.CallOption) (*QueryContractStateResponse, error) {
	out := new(QueryContractStateResponse)
	err := c.cc.Invoke(ctx, "/aegisvm.Query/ContractState", in, out, opts...)
	if err != nil {
		return nil, err
	}
	return out, nil
}

func (c *queryClient) Bytecode(ctx context.Context, in *QueryBytecodeRequest, opts ...grpc.CallOption) (*QueryBytecodeResponse, error) {
	out := new(QueryBytecodeResponse)
	err := c.cc.Invoke(ctx, "/aegisvm.Query/Bytecode", in, out, opts...)
	if err != nil {
		return nil, err
	}
	return out, nil
}

func (c *queryClient) IsCodeApproved(ctx context.Context, in *QueryIsCodeApprovedRequest, opts ...grpc.CallOption) (*QueryIsCodeApprovedResponse, error) {
	out := new(QueryIsCodeApprovedResponse)
	err := c.cc.Invoke(ctx, "/aegisvm.Query/IsCodeApproved", in, out, opts...)
	if err != nil {
		return nil, err
	}
	return out, nil
}

func (c *queryClient) Contracts(ctx context.Context, in *QueryContractsRequest, opts ...grpc.CallOption) (*QueryContractsResponse, error) {
	out := new(QueryContractsResponse)
	err := c.cc.Invoke(ctx, "/aegisvm.Query/Contracts", in, out, opts...)
	if err != nil {
		return nil, err
	}
	return out, nil
}

func (c *queryClient) SimulateExecution(ctx context.Context, in *QuerySimulateExecutionRequest, opts ...grpc.CallOption) (*QuerySimulateExecutionResponse, error) {
	out := new(QuerySimulateExecutionResponse)
	err := c.cc.Invoke(ctx, "/aegisvm.Query/SimulateExecution", in, out, opts...)
	if err != nil {
		return nil, err
	}
	return out, nil
}
