// Message server implementation for x/aegisvm

package keeper

import (
	"context"

	sdk "github.com/cosmos/cosmos-sdk/types"

	"github.com/aegisvm/cosmos/pkg/pqcrypto"
	"github.com/aegisvm/cosmos/pkg/vmbridge"
	"github.com/aegisvm/cosmos/x/aegisvm/types"
)

type msgServer struct {
	Keeper
}

// NewMsgServerImpl creates a new message server
func NewMsgServerImpl(keeper Keeper) types.MsgServer {
	return &msgServer{Keeper: keeper}
}

var _ types.MsgServer = msgServer{}

// DeployContract handles MsgDeployContract
func (k msgServer) DeployContract(goCtx context.Context, msg *types.MsgDeployContract) (*types.MsgDeployContractResponse, error) {
	ctx := sdk.UnwrapSDKContext(goCtx)

	// Validate sender
	sender, err := sdk.AccAddressFromBech32(msg.Sender)
	if err != nil {
		return nil, types.Wrap(types.ErrInvalidAddress, "sender")
	}

	// Check bytecode size
	params, err := k.GetParams(goCtx)
	if err != nil {
		return nil, err
	}
	if uint64(len(msg.Bytecode)) > params.MaxBytecodeSize {
		return nil, types.ErrCodeTooLarge
	}

	// Compute code hash
	codeHash, err := pqcrypto.SHA3_256(msg.Bytecode)
	if err != nil {
		return nil, types.Wrap(types.ErrInvalidBytecode, "hash failed")
	}

	// Check if code is approved (if governance required)
	if !k.IsCodeApproved(goCtx, codeHash[:]) {
		return nil, types.ErrUnapprovedCode
	}

	// Validate metadata
	metadata := types.ContractMetadata{
		Profile:   msg.Profile,
		ProofHash: msg.ProofHash,
		KATHash:   msg.KATHash,
	}
	if err := types.ValidateMetadata(metadata); err != nil {
		return nil, err
	}

	// Derive contract address
	contractAddr, err := k.DeriveContractAddress(goCtx, sender)
	if err != nil {
		return nil, types.Wrap(types.ErrInvalidAddress, "derive failed")
	}

	// Check contract doesn't already exist
	if _, found := k.GetContract(goCtx, contractAddr); found {
		return nil, types.ErrContractExists
	}

	// Store bytecode (deduplicated by hash)
	if _, found := k.GetBytecode(goCtx, codeHash[:]); !found {
		k.SetBytecode(goCtx, codeHash[:], msg.Bytecode)
	}

	// Create contract
	contract := types.Contract{
		Address:   contractAddr,
		Creator:   sender.Bytes(),
		CodeHash:  codeHash[:],
		Metadata:  metadata,
		CreatedAt: ctx.BlockHeight(),
		UpdatedAt: ctx.BlockHeight(),
	}

	// Store contract
	k.SetContract(goCtx, contract)

	// Execute init function if args provided
	if len(msg.InitArgs) > 0 {
		result, err := k.ExecuteContract(goCtx, contractAddr, sender.Bytes(), []byte{0, 0, 0, 0}, msg.InitArgs, params.MaxGasLimit)
		if err != nil {
			// Rollback contract deployment
			k.DeleteContract(goCtx, contractAddr)
			return nil, types.Wrap(types.ErrExecutionFailed, err.Error())
		}

		// Store initial state
		if len(result.State) > 0 {
			k.SetContractState(goCtx, contractAddr, result.State)
		}
	}

	// Emit event
	ctx.EventManager().EmitEvent(
		sdk.NewEvent(
			"deploy_contract",
			sdk.NewAttribute("contract", sdk.AccAddress(contractAddr).String()),
			sdk.NewAttribute("creator", msg.Sender),
			sdk.NewAttribute("code_hash", string(codeHash[:])),
		),
	)

	k.Logger(goCtx).Info("contract deployed",
		"address", sdk.AccAddress(contractAddr).String(),
		"creator", msg.Sender,
	)

	return &types.MsgDeployContractResponse{
		ContractAddress: contractAddr,
		CodeHash:        codeHash[:],
	}, nil
}

// ExecuteContract handles MsgExecuteContract
func (k msgServer) Execute(goCtx context.Context, msg *types.MsgExecuteContract) (*types.MsgExecuteContractResponse, error) {
	ctx := sdk.UnwrapSDKContext(goCtx)

	// Validate sender
	sender, err := sdk.AccAddressFromBech32(msg.Sender)
	if err != nil {
		return nil, types.Wrap(types.ErrInvalidAddress, "sender")
	}

	// Validate contract address
	contractAddr, err := sdk.AccAddressFromBech32(msg.Contract)
	if err != nil {
		return nil, types.Wrap(types.ErrInvalidAddress, "contract")
	}

	// Get contract
	contract, found := k.GetContract(goCtx, contractAddr.Bytes())
	if !found {
		return nil, types.ErrContractNotFound
	}

	// Transfer funds if any
	if !msg.Funds.IsZero() {
		if err := k.bankKeeper.SendCoins(goCtx, sender, contractAddr, msg.Funds); err != nil {
			return nil, types.Wrap(types.ErrInsufficientFunds, err.Error())
		}
	}

	// Get gas limit from remaining gas
	params, err := k.GetParams(goCtx)
	if err != nil {
		return nil, err
	}
	gasLimit := ctx.GasMeter().Limit() - ctx.GasMeter().GasConsumed()
	if gasLimit > params.MaxGasLimit {
		gasLimit = params.MaxGasLimit
	}

	// Apply gas discount based on certification level
	discount := types.GasDiscount(contract.Metadata.CertificationLevel)
	effectiveGasLimit := gasLimit * (100 + discount) / 100

	// Execute contract
	result, err := k.ExecuteContract(goCtx, contractAddr.Bytes(), sender.Bytes(), msg.Function, msg.Args, effectiveGasLimit)
	if err != nil {
		return nil, types.Wrap(types.ErrExecutionFailed, err.Error())
	}

	if !result.Success {
		return nil, types.ErrContractReverted
	}

	// Update state
	if len(result.State) > 0 {
		k.SetContractState(goCtx, contractAddr.Bytes(), result.State)
	}

	// Consume gas
	ctx.GasMeter().ConsumeGas(result.GasUsed, "contract execution")

	// Emit events for logs
	for _, logData := range result.Logs {
		ctx.EventManager().EmitEvent(
			sdk.NewEvent(
				"contract_log",
				sdk.NewAttribute("contract", msg.Contract),
				sdk.NewAttribute("data", string(logData)),
			),
		)
	}

	k.Logger(goCtx).Info("contract executed",
		"contract", msg.Contract,
		"sender", msg.Sender,
		"gas_used", result.GasUsed,
	)

	return &types.MsgExecuteContractResponse{
		Data:    result.ReturnData,
		GasUsed: result.GasUsed,
		Logs:    result.Logs,
	}, nil
}

// MigrateContract handles MsgMigrateContract
func (k msgServer) Migrate(goCtx context.Context, msg *types.MsgMigrateContract) (*types.MsgMigrateContractResponse, error) {
	ctx := sdk.UnwrapSDKContext(goCtx)

	// Validate sender
	sender, err := sdk.AccAddressFromBech32(msg.Sender)
	if err != nil {
		return nil, types.Wrap(types.ErrInvalidAddress, "sender")
	}

	// Validate contract address
	contractAddr, err := sdk.AccAddressFromBech32(msg.Contract)
	if err != nil {
		return nil, types.Wrap(types.ErrInvalidAddress, "contract")
	}

	// Get contract
	contract, found := k.GetContract(goCtx, contractAddr.Bytes())
	if !found {
		return nil, types.ErrContractNotFound
	}

	// TODO: Check admin authorization
	_ = sender

	// Compute new code hash
	newCodeHash, err := pqcrypto.SHA3_256(msg.NewBytecode)
	if err != nil {
		return nil, types.Wrap(types.ErrInvalidBytecode, "hash failed")
	}

	// Check if new code is approved
	if !k.IsCodeApproved(goCtx, newCodeHash[:]) {
		return nil, types.ErrUnapprovedCode
	}

	// Store new bytecode
	if _, found := k.GetBytecode(goCtx, newCodeHash[:]); !found {
		k.SetBytecode(goCtx, newCodeHash[:], msg.NewBytecode)
	}

	// Update contract
	contract.CodeHash = newCodeHash[:]
	contract.Metadata.ProofHash = msg.NewProofHash
	contract.UpdatedAt = ctx.BlockHeight()
	k.SetContract(goCtx, contract)

	// Execute migrate function if args provided
	if len(msg.MigrateArgs) > 0 {
		params, err := k.GetParams(goCtx)
		if err != nil {
			return nil, err
		}
		_, err = k.ExecuteContract(goCtx, contractAddr.Bytes(), sender.Bytes(), []byte{0, 0, 0, 1}, msg.MigrateArgs, params.MaxGasLimit)
		if err != nil {
			return nil, types.Wrap(types.ErrExecutionFailed, err.Error())
		}
	}

	// Emit event
	ctx.EventManager().EmitEvent(
		sdk.NewEvent(
			"migrate_contract",
			sdk.NewAttribute("contract", msg.Contract),
			sdk.NewAttribute("sender", msg.Sender),
			sdk.NewAttribute("new_code_hash", string(newCodeHash[:])),
		),
	)

	k.Logger(goCtx).Info("contract migrated",
		"contract", msg.Contract,
		"new_code_hash", string(newCodeHash[:]),
	)

	return &types.MsgMigrateContractResponse{
		NewCodeHash: newCodeHash[:],
	}, nil
}

// UpdateAdmin handles MsgUpdateAdmin
func (k msgServer) UpdateAdmin(goCtx context.Context, msg *types.MsgUpdateAdmin) (*types.MsgUpdateAdminResponse, error) {
	ctx := sdk.UnwrapSDKContext(goCtx)

	// Validate sender
	sender, err := sdk.AccAddressFromBech32(msg.Sender)
	if err != nil {
		return nil, types.Wrap(types.ErrInvalidAddress, "sender")
	}

	// Validate contract address
	contractAddr, err := sdk.AccAddressFromBech32(msg.Contract)
	if err != nil {
		return nil, types.Wrap(types.ErrInvalidAddress, "contract")
	}

	// Get contract
	contract, found := k.GetContract(goCtx, contractAddr.Bytes())
	if !found {
		return nil, types.ErrContractNotFound
	}

	// TODO: Check admin authorization
	_ = sender
	_ = contract

	// Emit event
	ctx.EventManager().EmitEvent(
		sdk.NewEvent(
			"update_admin",
			sdk.NewAttribute("contract", msg.Contract),
			sdk.NewAttribute("sender", msg.Sender),
			sdk.NewAttribute("new_admin", msg.NewAdmin),
		),
	)

	k.Logger(goCtx).Info("contract admin updated",
		"contract", msg.Contract,
		"new_admin", msg.NewAdmin,
	)

	return &types.MsgUpdateAdminResponse{}, nil
}

// ExecuteContract is the internal contract execution function
// This function bridges Cosmos SDK to the formally verified SPARK/Ada VM via CGO
func (k Keeper) ExecuteContract(ctx context.Context, contractAddr, caller, function, args []byte, gasLimit uint64) (*types.ExecutionResult, error) {
	sdkCtx := sdk.UnwrapSDKContext(ctx)

	// Get contract
	contract, found := k.GetContract(ctx, contractAddr)
	if !found {
		return nil, types.ErrContractNotFound
	}

	// Get bytecode
	bytecode, found := k.GetBytecode(ctx, contract.CodeHash)
	if !found {
		return nil, types.ErrInvalidBytecode
	}

	// Get current state
	state, _ := k.GetContractState(ctx, contractAddr)

	// Build execution context for VM bridge
	execCtx := vmbridge.ExecutionContext{
		GasLimit:     gasLimit,
		GasPrice:     1, // Could be parameterized
		BlockHeight:  sdkCtx.BlockHeight(),
		BlockTime:    sdkCtx.BlockTime().Unix(),
		ChainID:      sdkCtx.ChainID(),
		CertLevel:    vmbridge.CertificationLevel(types.CertLevelToUint8(contract.Metadata.CertificationLevel)),
		IsStaticCall: false,
	}

	// Copy addresses
	copy(execCtx.Origin[:], caller)
	copy(execCtx.Caller[:], caller)
	copy(execCtx.ContractAddress[:], contractAddr)

	// Create VM instance
	vm, err := vmbridge.NewVM()
	if err != nil {
		k.Logger(ctx).Error("failed to create VM instance", "error", err)
		return nil, types.Wrap(types.ErrExecutionFailed, "vm creation failed")
	}
	defer vm.Close()

	// Load bytecode into VM
	if err := vm.LoadBytecode(bytecode); err != nil {
		k.Logger(ctx).Error("failed to load bytecode", "error", err)
		return nil, types.Wrap(types.ErrInvalidBytecode, err.Error())
	}

	// Set initial state
	if len(state) > 0 {
		// Parse state entries and set each one
		// State format: [key_len:4][key][value_len:4][value]...
		offset := 0
		for offset < len(state) {
			if offset+4 > len(state) {
				break
			}
			keyLen := int(state[offset])<<24 | int(state[offset+1])<<16 | int(state[offset+2])<<8 | int(state[offset+3])
			offset += 4

			if offset+keyLen > len(state) {
				break
			}
			key := state[offset : offset+keyLen]
			offset += keyLen

			if offset+4 > len(state) {
				break
			}
			valueLen := int(state[offset])<<24 | int(state[offset+1])<<16 | int(state[offset+2])<<8 | int(state[offset+3])
			offset += 4

			if offset+valueLen > len(state) {
				break
			}
			value := state[offset : offset+valueLen]
			offset += valueLen

			if err := vm.SetState(key, value); err != nil {
				k.Logger(ctx).Error("failed to set state", "key_len", keyLen, "error", err)
			}
		}
	}

	// Execute via CGO bridge to SPARK/Ada VM
	vmResult, err := vm.Execute(execCtx, function, args, state)
	if err != nil {
		k.Logger(ctx).Error("vm execution failed", "error", err, "contract", contractAddr)

		// Return partial result with error info
		return &types.ExecutionResult{
			Success:    false,
			ReturnData: []byte{},
			GasUsed:    vmResult.GasUsed,
			State:      state,
			Logs:       [][]byte{},
		}, types.Wrap(types.ErrExecutionFailed, err.Error())
	}

	// Convert state changes to serialized format
	var newState []byte
	for _, change := range vmResult.StateChanges {
		if change.IsDelete {
			continue // Handle deletions separately
		}
		// Encode: [key_len:4][key][value_len:4][value]
		keyLen := len(change.Key)
		valueLen := len(change.Value)

		newState = append(newState, byte(keyLen>>24), byte(keyLen>>16), byte(keyLen>>8), byte(keyLen))
		newState = append(newState, change.Key...)
		newState = append(newState, byte(valueLen>>24), byte(valueLen>>16), byte(valueLen>>8), byte(valueLen))
		newState = append(newState, change.Value...)
	}

	// If no state changes, preserve existing state
	if len(newState) == 0 {
		newState = state
	}

	// Convert logs
	var logs [][]byte
	for _, log := range vmResult.Logs {
		logData := make([]byte, 0, 20+len(log.Topics)*32+len(log.Data))
		logData = append(logData, log.Address[:]...)
		for _, topic := range log.Topics {
			logData = append(logData, topic[:]...)
		}
		logData = append(logData, log.Data...)
		logs = append(logs, logData)
	}

	k.Logger(ctx).Debug("contract execution completed",
		"contract", contractAddr,
		"gas_used", vmResult.GasUsed,
		"success", vmResult.Success,
	)

	return &types.ExecutionResult{
		Success:    vmResult.Success,
		ReturnData: vmResult.ReturnData,
		GasUsed:    vmResult.GasUsed,
		State:      newState,
		Logs:       logs,
	}, nil
}
