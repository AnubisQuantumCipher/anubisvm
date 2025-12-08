// Query server implementation for x/aegisvm

package keeper

import (
	"context"

	sdk "github.com/cosmos/cosmos-sdk/types"

	"github.com/aegisvm/cosmos/x/aegisvm/types"
)

type queryServer struct {
	Keeper
}

// NewQueryServerImpl creates a new query server
func NewQueryServerImpl(keeper Keeper) types.QueryServer {
	return &queryServer{Keeper: keeper}
}

var _ types.QueryServer = queryServer{}

// Params returns the module parameters
func (k queryServer) Params(goCtx context.Context, req *types.QueryParamsRequest) (*types.QueryParamsResponse, error) {
	params, err := k.GetParams(goCtx)
	if err != nil {
		return nil, err
	}
	return &types.QueryParamsResponse{Params: params}, nil
}

// Contract returns a contract by address
func (k queryServer) Contract(goCtx context.Context, req *types.QueryContractRequest) (*types.QueryContractResponse, error) {
	contractAddr, err := sdk.AccAddressFromBech32(req.Address)
	if err != nil {
		return nil, types.Wrap(types.ErrInvalidAddress, err.Error())
	}

	contract, found := k.GetContract(goCtx, contractAddr.Bytes())
	if !found {
		return nil, types.ErrContractNotFound
	}

	return &types.QueryContractResponse{Contract: contract}, nil
}

// ContractState returns the state of a contract
func (k queryServer) ContractState(goCtx context.Context, req *types.QueryContractStateRequest) (*types.QueryContractStateResponse, error) {
	contractAddr, err := sdk.AccAddressFromBech32(req.Address)
	if err != nil {
		return nil, types.Wrap(types.ErrInvalidAddress, err.Error())
	}

	state, found := k.GetContractState(goCtx, contractAddr.Bytes())
	if !found {
		state = []byte{}
	}

	return &types.QueryContractStateResponse{State: state}, nil
}

// Bytecode returns bytecode by code hash
func (k queryServer) Bytecode(goCtx context.Context, req *types.QueryBytecodeRequest) (*types.QueryBytecodeResponse, error) {
	bytecode, found := k.GetBytecode(goCtx, req.CodeHash)
	if !found {
		return nil, types.ErrInvalidBytecode
	}

	return &types.QueryBytecodeResponse{Bytecode: bytecode}, nil
}

// IsCodeApproved checks if a code hash is approved
func (k queryServer) IsCodeApproved(goCtx context.Context, req *types.QueryIsCodeApprovedRequest) (*types.QueryIsCodeApprovedResponse, error) {
	approved := k.Keeper.IsCodeApproved(goCtx, req.CodeHash)

	return &types.QueryIsCodeApprovedResponse{Approved: approved}, nil
}

// Contracts returns all contracts (paginated)
func (k queryServer) Contracts(goCtx context.Context, req *types.QueryContractsRequest) (*types.QueryContractsResponse, error) {
	var contracts []types.Contract
	k.IterateContracts(goCtx, func(contract types.Contract) bool {
		contracts = append(contracts, contract)
		return false
	})

	return &types.QueryContractsResponse{Contracts: contracts}, nil
}

// SimulateExecution simulates contract execution without state changes
func (k queryServer) SimulateExecution(goCtx context.Context, req *types.QuerySimulateExecutionRequest) (*types.QuerySimulateExecutionResponse, error) {
	contractAddr, err := sdk.AccAddressFromBech32(req.Contract)
	if err != nil {
		return nil, types.Wrap(types.ErrInvalidAddress, err.Error())
	}

	callerAddr, err := sdk.AccAddressFromBech32(req.Sender)
	if err != nil {
		return nil, types.Wrap(types.ErrInvalidAddress, err.Error())
	}

	// Get params for gas limit
	params, err := k.GetParams(goCtx)
	if err != nil {
		return nil, err
	}

	// Execute in simulation mode (no state changes)
	result, err := k.ExecuteContract(goCtx, contractAddr.Bytes(), callerAddr.Bytes(), req.Function, req.Args, params.MaxGasLimit)
	if err != nil {
		return nil, err
	}

	return &types.QuerySimulateExecutionResponse{
		Result: *result,
	}, nil
}
