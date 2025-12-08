// Message server interface for x/aegisvm

package types

import (
	context "context"
)

// MsgServer is the server API for aegisvm module
type MsgServer interface {
	// DeployContract deploys a new smart contract
	DeployContract(context.Context, *MsgDeployContract) (*MsgDeployContractResponse, error)

	// Execute executes a contract function
	Execute(context.Context, *MsgExecuteContract) (*MsgExecuteContractResponse, error)

	// Migrate migrates a contract to new bytecode
	Migrate(context.Context, *MsgMigrateContract) (*MsgMigrateContractResponse, error)

	// UpdateAdmin updates the contract admin
	UpdateAdmin(context.Context, *MsgUpdateAdmin) (*MsgUpdateAdminResponse, error)
}

// MsgDeployContractResponse is the response for MsgDeployContract
type MsgDeployContractResponse struct {
	// ContractAddress is the deployed contract address
	ContractAddress []byte `json:"contract_address"`

	// CodeHash is the SHA3-256 of the bytecode
	CodeHash []byte `json:"code_hash"`
}

// MsgExecuteContractResponse is the response for MsgExecuteContract
type MsgExecuteContractResponse struct {
	// Data is the return data from execution
	Data []byte `json:"data,omitempty"`

	// GasUsed is the amount of gas consumed
	GasUsed uint64 `json:"gas_used"`

	// Logs are events emitted during execution
	Logs [][]byte `json:"logs,omitempty"`
}

// MsgMigrateContractResponse is the response for MsgMigrateContract
type MsgMigrateContractResponse struct {
	// NewCodeHash is the SHA3-256 of the new bytecode
	NewCodeHash []byte `json:"new_code_hash"`
}

// MsgUpdateAdminResponse is the response for MsgUpdateAdmin
type MsgUpdateAdminResponse struct{}
