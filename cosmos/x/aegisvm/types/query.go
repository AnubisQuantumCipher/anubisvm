// Query types for x/aegisvm

package types

import (
	context "context"
	"fmt"
)

// Proto interface implementations for query types
func (m *QueryParamsRequest) Reset()         { *m = QueryParamsRequest{} }
func (m *QueryParamsRequest) String() string { return "QueryParamsRequest" }
func (m *QueryParamsRequest) ProtoMessage()  {}

func (m *QueryParamsResponse) Reset()         { *m = QueryParamsResponse{} }
func (m *QueryParamsResponse) String() string { return fmt.Sprintf("%+v", *m) }
func (m *QueryParamsResponse) ProtoMessage()  {}

func (m *QueryContractRequest) Reset()         { *m = QueryContractRequest{} }
func (m *QueryContractRequest) String() string { return m.Address }
func (m *QueryContractRequest) ProtoMessage()  {}

func (m *QueryContractResponse) Reset()         { *m = QueryContractResponse{} }
func (m *QueryContractResponse) String() string { return fmt.Sprintf("%+v", *m) }
func (m *QueryContractResponse) ProtoMessage()  {}

func (m *QueryContractStateRequest) Reset()         { *m = QueryContractStateRequest{} }
func (m *QueryContractStateRequest) String() string { return m.Address }
func (m *QueryContractStateRequest) ProtoMessage()  {}

func (m *QueryContractStateResponse) Reset()         { *m = QueryContractStateResponse{} }
func (m *QueryContractStateResponse) String() string { return fmt.Sprintf("%+v", *m) }
func (m *QueryContractStateResponse) ProtoMessage()  {}

func (m *QueryBytecodeRequest) Reset()         { *m = QueryBytecodeRequest{} }
func (m *QueryBytecodeRequest) String() string { return fmt.Sprintf("%x", m.CodeHash) }
func (m *QueryBytecodeRequest) ProtoMessage()  {}

func (m *QueryBytecodeResponse) Reset()         { *m = QueryBytecodeResponse{} }
func (m *QueryBytecodeResponse) String() string { return fmt.Sprintf("%d bytes", len(m.Bytecode)) }
func (m *QueryBytecodeResponse) ProtoMessage()  {}

func (m *QueryIsCodeApprovedRequest) Reset()         { *m = QueryIsCodeApprovedRequest{} }
func (m *QueryIsCodeApprovedRequest) String() string { return fmt.Sprintf("%x", m.CodeHash) }
func (m *QueryIsCodeApprovedRequest) ProtoMessage()  {}

func (m *QueryIsCodeApprovedResponse) Reset()         { *m = QueryIsCodeApprovedResponse{} }
func (m *QueryIsCodeApprovedResponse) String() string { return fmt.Sprintf("%t", m.Approved) }
func (m *QueryIsCodeApprovedResponse) ProtoMessage()  {}

func (m *QueryContractsRequest) Reset()         { *m = QueryContractsRequest{} }
func (m *QueryContractsRequest) String() string { return "QueryContractsRequest" }
func (m *QueryContractsRequest) ProtoMessage()  {}

func (m *QueryContractsResponse) Reset()         { *m = QueryContractsResponse{} }
func (m *QueryContractsResponse) String() string { return fmt.Sprintf("%d contracts", len(m.Contracts)) }
func (m *QueryContractsResponse) ProtoMessage()  {}

func (m *QuerySimulateExecutionRequest) Reset()         { *m = QuerySimulateExecutionRequest{} }
func (m *QuerySimulateExecutionRequest) String() string { return m.Contract }
func (m *QuerySimulateExecutionRequest) ProtoMessage()  {}

func (m *QuerySimulateExecutionResponse) Reset()         { *m = QuerySimulateExecutionResponse{} }
func (m *QuerySimulateExecutionResponse) String() string { return fmt.Sprintf("%+v", m.Result) }
func (m *QuerySimulateExecutionResponse) ProtoMessage()  {}

// QueryServer is the query API for aegisvm module
type QueryServer interface {
	// Params returns the module parameters
	Params(context.Context, *QueryParamsRequest) (*QueryParamsResponse, error)

	// Contract returns a contract by address
	Contract(context.Context, *QueryContractRequest) (*QueryContractResponse, error)

	// ContractState returns the state of a contract
	ContractState(context.Context, *QueryContractStateRequest) (*QueryContractStateResponse, error)

	// Bytecode returns bytecode by code hash
	Bytecode(context.Context, *QueryBytecodeRequest) (*QueryBytecodeResponse, error)

	// IsCodeApproved checks if a code hash is approved
	IsCodeApproved(context.Context, *QueryIsCodeApprovedRequest) (*QueryIsCodeApprovedResponse, error)

	// Contracts returns all contracts
	Contracts(context.Context, *QueryContractsRequest) (*QueryContractsResponse, error)

	// SimulateExecution simulates contract execution
	SimulateExecution(context.Context, *QuerySimulateExecutionRequest) (*QuerySimulateExecutionResponse, error)
}

// QueryParamsRequest is the request for Params query
type QueryParamsRequest struct{}

// QueryParamsResponse is the response for Params query
type QueryParamsResponse struct {
	Params Params `json:"params"`
}

// QueryContractRequest is the request for Contract query
type QueryContractRequest struct {
	Address string `json:"address"`
}

// QueryContractResponse is the response for Contract query
type QueryContractResponse struct {
	Contract Contract `json:"contract"`
}

// QueryContractStateRequest is the request for ContractState query
type QueryContractStateRequest struct {
	Address string `json:"address"`
}

// QueryContractStateResponse is the response for ContractState query
type QueryContractStateResponse struct {
	State []byte `json:"state"`
}

// QueryBytecodeRequest is the request for Bytecode query
type QueryBytecodeRequest struct {
	CodeHash []byte `json:"code_hash"`
}

// QueryBytecodeResponse is the response for Bytecode query
type QueryBytecodeResponse struct {
	Bytecode []byte `json:"bytecode"`
}

// QueryIsCodeApprovedRequest is the request for IsCodeApproved query
type QueryIsCodeApprovedRequest struct {
	CodeHash []byte `json:"code_hash"`
}

// QueryIsCodeApprovedResponse is the response for IsCodeApproved query
type QueryIsCodeApprovedResponse struct {
	Approved bool `json:"approved"`
}

// QueryContractsRequest is the request for Contracts query
type QueryContractsRequest struct {
	// Pagination can be added later
}

// QueryContractsResponse is the response for Contracts query
type QueryContractsResponse struct {
	Contracts []Contract `json:"contracts"`
}

// QuerySimulateExecutionRequest is the request for SimulateExecution query
type QuerySimulateExecutionRequest struct {
	Contract string `json:"contract"`
	Sender   string `json:"sender"`
	Function []byte `json:"function"`
	Args     []byte `json:"args,omitempty"`
}

// QuerySimulateExecutionResponse is the response for SimulateExecution query
type QuerySimulateExecutionResponse struct {
	Result ExecutionResult `json:"result"`
}
