// Message server interface for x/anubis

package types

import (
	context "context"
)

// MsgServer is the server API for anubis module
type MsgServer interface {
	// === Gate: Private Execution ===

	// PrivateExecute executes a contract privately
	PrivateExecute(context.Context, *MsgPrivateExecute) (*MsgPrivateExecuteResponse, error)

	// EnablePrivate enables private execution for a contract
	EnablePrivate(context.Context, *MsgEnablePrivate) (*MsgEnablePrivateResponse, error)

	// SubmitProof submits a ZK proof for private execution
	SubmitProof(context.Context, *MsgSubmitProof) (*MsgSubmitProofResponse, error)

	// InitSession initializes a private execution session
	InitSession(context.Context, *MsgInitSession) (*MsgInitSessionResponse, error)

	// CloseSession closes a private execution session
	CloseSession(context.Context, *MsgCloseSession) (*MsgCloseSessionResponse, error)

	// SessionExecute executes within a session
	SessionExecute(context.Context, *MsgSessionExecute) (*MsgSessionExecuteResponse, error)

	// === Whisper: Confidential Transactions ===

	// ConfidentialTransfer performs a confidential transfer
	ConfidentialTransfer(context.Context, *MsgConfidentialTransfer) (*MsgConfidentialTransferResponse, error)

	// ShieldDeposit shields public funds into a confidential note
	ShieldDeposit(context.Context, *MsgShieldDeposit) (*MsgShieldDepositResponse, error)

	// UnshieldWithdraw withdraws from a confidential note to public balance
	UnshieldWithdraw(context.Context, *MsgUnshieldWithdraw) (*MsgUnshieldWithdrawResponse, error)

	// === Eye: Selective Disclosure ===

	// CreateDisclosure creates a selective disclosure for a recipient
	CreateDisclosure(context.Context, *MsgCreateDisclosure) (*MsgCreateDisclosureResponse, error)

	// RevokeDisclosure revokes an existing disclosure
	RevokeDisclosure(context.Context, *MsgRevokeDisclosure) (*MsgRevokeDisclosureResponse, error)

	// === Prover Network ===

	// RegisterProver registers a new ZK prover
	RegisterProver(context.Context, *MsgRegisterProver) (*MsgRegisterProverResponse, error)

	// UnbondProver initiates unbonding for a prover
	UnbondProver(context.Context, *MsgUnbondProver) (*MsgUnbondProverResponse, error)
}

// RegisterMsgServer registers the message server
func RegisterMsgServer(s interface{}, srv MsgServer) {
	// gRPC server registration (placeholder)
}
