// Message server implementation for x/anubis

package keeper

import (
	"context"
	"encoding/hex"
	"fmt"

	sdk "github.com/cosmos/cosmos-sdk/types"

	"github.com/aegisvm/cosmos/x/anubis/types"
)

type msgServer struct {
	Keeper
}

// NewMsgServerImpl creates a new message server
func NewMsgServerImpl(keeper Keeper) types.MsgServer {
	return &msgServer{Keeper: keeper}
}

var _ types.MsgServer = msgServer{}

// PrivateExecute handles MsgPrivateExecute
func (k msgServer) PrivateExecute(goCtx context.Context, msg *types.MsgPrivateExecute) (*types.MsgPrivateExecuteResponse, error) {
	ctx := sdk.UnwrapSDKContext(goCtx)

	params, err := k.GetParams(goCtx)
	if err != nil {
		return nil, err
	}
	if !params.PrivateExecEnabled {
		return nil, types.ErrPrivateExecDisabled
	}

	// Validate sender
	sender, err := sdk.AccAddressFromBech32(msg.Sender)
	if err != nil {
		return nil, types.Wrap(types.ErrInvalidAddress, "sender")
	}

	// Validate encrypted call
	if err := types.ValidateEncryptedCall(msg.EncryptedCall); err != nil {
		return nil, err
	}

	// Check if contract supports private execution
	pc, found := k.GetPrivateContract(goCtx, msg.EncryptedCall.ContractAddress)
	if !found || !pc.Enabled {
		return nil, types.ErrContractNotPrivate
	}

	// Check if caller is authorized
	if !k.IsCallerAuthorized(goCtx, msg.EncryptedCall.ContractAddress, sender.Bytes()) {
		return nil, types.ErrUnauthorizedCaller
	}

	// Generate call ID
	callID, err := k.GenerateCallID(goCtx, sender.Bytes(), msg.EncryptedCall.Nonce)
	if err != nil {
		return nil, err
	}

	// Store the encrypted call for later proof submission
	encryptedCall := msg.EncryptedCall
	encryptedCall.CallID = callID
	encryptedCall.Sender = sender.Bytes()
	k.SetEncryptedCall(goCtx, encryptedCall)

	// Emit event
	ctx.EventManager().EmitEvent(
		sdk.NewEvent(
			"private_execute",
			sdk.NewAttribute("call_id", string(callID)),
			sdk.NewAttribute("sender", msg.Sender),
		),
	)

	k.Logger(goCtx).Info("private execution requested",
		"call_id", string(callID),
		"sender", msg.Sender,
	)

	return &types.MsgPrivateExecuteResponse{
		CallID: callID,
	}, nil
}

// EnablePrivate handles MsgEnablePrivate
func (k msgServer) EnablePrivate(goCtx context.Context, msg *types.MsgEnablePrivate) (*types.MsgEnablePrivateResponse, error) {
	ctx := sdk.UnwrapSDKContext(goCtx)

	params, err := k.GetParams(goCtx)
	if err != nil {
		return nil, err
	}
	if !params.PrivateExecEnabled {
		return nil, types.ErrPrivateExecDisabled
	}

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

	// Check contract exists
	_, found := k.aegisvmKeeper.GetContract(goCtx, contractAddr.Bytes())
	if !found {
		return nil, types.Wrap(types.ErrContractNotPrivate, "contract not found")
	}

	// TODO: Verify sender is contract admin

	// Convert authorized callers to bytes
	var authorizedCallers [][]byte
	for _, callerStr := range msg.AuthorizedCallers {
		callerAddr, err := sdk.AccAddressFromBech32(callerStr)
		if err != nil {
			return nil, types.Wrap(types.ErrInvalidAddress, "authorized caller")
		}
		authorizedCallers = append(authorizedCallers, callerAddr.Bytes())
	}

	// Create private contract configuration
	pc := types.PrivateContract{
		Address:           contractAddr.Bytes(),
		PublicKey:         msg.PublicKey,
		EncryptionScheme:  msg.EncryptionScheme,
		AuthorizedCallers: authorizedCallers,
		MaxPrivateCalls:   msg.MaxPrivateCalls,
		Enabled:           true,
	}

	if err := types.ValidatePrivateContract(pc); err != nil {
		return nil, err
	}

	k.SetPrivateContract(goCtx, pc)

	// Emit event
	ctx.EventManager().EmitEvent(
		sdk.NewEvent(
			"enable_private",
			sdk.NewAttribute("contract", msg.Contract),
			sdk.NewAttribute("sender", msg.Sender),
		),
	)

	k.Logger(goCtx).Info("private execution enabled",
		"contract", msg.Contract,
		"sender", sender.String(),
	)

	return &types.MsgEnablePrivateResponse{}, nil
}

// SubmitProof handles MsgSubmitProof
func (k msgServer) SubmitProof(goCtx context.Context, msg *types.MsgSubmitProof) (*types.MsgSubmitProofResponse, error) {
	ctx := sdk.UnwrapSDKContext(goCtx)

	// Validate sender
	_, err := sdk.AccAddressFromBech32(msg.Sender)
	if err != nil {
		return nil, types.Wrap(types.ErrInvalidAddress, "sender")
	}

	// Get the encrypted call
	ec, found := k.GetEncryptedCall(goCtx, msg.CallID)
	if !found {
		return nil, types.ErrCallNotFound
	}

	// Verify ZK proof
	if err := k.VerifyZKProof(goCtx, msg.Proof); err != nil {
		return nil, err
	}

	// Store the proof
	k.SetZKProof(goCtx, msg.Proof)

	// Delete the encrypted call (it's been processed)
	k.DeleteEncryptedCall(goCtx, msg.CallID)

	// Update contract state if state root is valid
	if len(msg.Proof.NewStateRoot) > 0 {
		k.aegisvmKeeper.SetContractState(goCtx, ec.ContractAddress, msg.Proof.NewStateRoot)
	}

	// Emit event
	ctx.EventManager().EmitEvent(
		sdk.NewEvent(
			"submit_proof",
			sdk.NewAttribute("call_id", string(msg.CallID)),
			sdk.NewAttribute("verified", "true"),
		),
	)

	k.Logger(goCtx).Info("ZK proof verified",
		"call_id", string(msg.CallID),
		"proof_hash", string(msg.Proof.ProofHash),
	)

	return &types.MsgSubmitProofResponse{
		Verified: true,
	}, nil
}

// InitSession handles MsgInitSession
func (k msgServer) InitSession(goCtx context.Context, msg *types.MsgInitSession) (*types.MsgInitSessionResponse, error) {
	ctx := sdk.UnwrapSDKContext(goCtx)

	params, err := k.GetParams(goCtx)
	if err != nil {
		return nil, err
	}
	if !params.PrivateExecEnabled {
		return nil, types.ErrPrivateExecDisabled
	}

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

	// Check if contract supports private execution
	pc, found := k.GetPrivateContract(goCtx, contractAddr.Bytes())
	if !found || !pc.Enabled {
		return nil, types.ErrContractNotPrivate
	}

	// Check if caller is authorized
	if !k.IsCallerAuthorized(goCtx, contractAddr.Bytes(), sender.Bytes()) {
		return nil, types.ErrUnauthorizedCaller
	}

	// Validate session parameters
	if msg.MaxCalls > params.MaxSessionCalls {
		return nil, types.Wrap(types.ErrMaxCallsExceeded, "exceeds maximum allowed")
	}
	if msg.Duration > params.MaxSessionDuration {
		msg.Duration = params.MaxSessionDuration
	}

	// Generate session ID
	sessionID, err := k.GenerateSessionID(goCtx, sender.Bytes(), contractAddr.Bytes())
	if err != nil {
		return nil, err
	}

	expiresAt := ctx.BlockHeight() + msg.Duration

	// Create session
	session := types.SessionKey{
		SessionID:       sessionID,
		ContractAddress: contractAddr.Bytes(),
		Initiator:       sender.Bytes(),
		EncryptedKey:    msg.KEMCiphertext, // Store the encapsulated key
		ExpiresAt:       expiresAt,
		CallCount:       0,
		MaxCalls:        msg.MaxCalls,
	}

	k.SetSession(goCtx, session)

	// Emit event
	ctx.EventManager().EmitEvent(
		sdk.NewEvent(
			"init_session",
			sdk.NewAttribute("session_id", string(sessionID)),
			sdk.NewAttribute("contract", msg.Contract),
			sdk.NewAttribute("sender", msg.Sender),
			sdk.NewAttribute("expires_at", fmt.Sprintf("%d", expiresAt)),
		),
	)

	k.Logger(goCtx).Info("session initialized",
		"session_id", string(sessionID),
		"contract", msg.Contract,
		"expires_at", expiresAt,
	)

	return &types.MsgInitSessionResponse{
		SessionID: sessionID,
		ExpiresAt: expiresAt,
	}, nil
}

// CloseSession handles MsgCloseSession
func (k msgServer) CloseSession(goCtx context.Context, msg *types.MsgCloseSession) (*types.MsgCloseSessionResponse, error) {
	ctx := sdk.UnwrapSDKContext(goCtx)

	// Validate sender
	sender, err := sdk.AccAddressFromBech32(msg.Sender)
	if err != nil {
		return nil, types.Wrap(types.ErrInvalidAddress, "sender")
	}

	// Get session
	session, found := k.GetSession(goCtx, msg.SessionID)
	if !found {
		return nil, types.Wrap(types.ErrCallNotFound, "session not found")
	}

	// Verify sender is session owner
	if !bytesEqual(session.Initiator, sender.Bytes()) {
		return nil, types.ErrUnauthorizedCaller
	}

	// Delete session
	k.DeleteSession(goCtx, msg.SessionID)

	// Emit event
	ctx.EventManager().EmitEvent(
		sdk.NewEvent(
			"close_session",
			sdk.NewAttribute("session_id", string(msg.SessionID)),
			sdk.NewAttribute("sender", msg.Sender),
		),
	)

	k.Logger(goCtx).Info("session closed",
		"session_id", string(msg.SessionID),
	)

	return &types.MsgCloseSessionResponse{}, nil
}

// SessionExecute handles MsgSessionExecute
func (k msgServer) SessionExecute(goCtx context.Context, msg *types.MsgSessionExecute) (*types.MsgSessionExecuteResponse, error) {
	ctx := sdk.UnwrapSDKContext(goCtx)

	params, err := k.GetParams(goCtx)
	if err != nil {
		return nil, err
	}
	if !params.PrivateExecEnabled {
		return nil, types.ErrPrivateExecDisabled
	}

	// Validate sender
	sender, err := sdk.AccAddressFromBech32(msg.Sender)
	if err != nil {
		return nil, types.Wrap(types.ErrInvalidAddress, "sender")
	}

	// Get session
	session, found := k.GetSession(goCtx, msg.SessionID)
	if !found {
		return nil, types.Wrap(types.ErrCallNotFound, "session not found")
	}

	// Verify sender is session owner
	if !bytesEqual(session.Initiator, sender.Bytes()) {
		return nil, types.ErrUnauthorizedCaller
	}

	// Check if session is expired
	if session.ExpiresAt <= ctx.BlockHeight() {
		k.DeleteSession(goCtx, msg.SessionID)
		return nil, types.ErrSessionExpired
	}

	// Check if session call limit reached
	if session.CallCount >= session.MaxCalls {
		return nil, types.ErrSessionLimitReached
	}

	// TODO: Decrypt and execute using session key
	// This would involve:
	// 1. Decapsulating the session key using ML-KEM
	// 2. Decrypting the function and args
	// 3. Executing the contract
	// 4. Encrypting the result

	// Increment call count
	session.CallCount++
	k.SetSession(goCtx, session)

	// Placeholder result
	encryptedResult := []byte{} // Would be encrypted execution result
	gasUsed := uint64(1000)

	// Emit event
	ctx.EventManager().EmitEvent(
		sdk.NewEvent(
			"session_execute",
			sdk.NewAttribute("session_id", string(msg.SessionID)),
			sdk.NewAttribute("call_count", fmt.Sprintf("%d", session.CallCount)),
		),
	)

	k.Logger(goCtx).Info("session execution completed",
		"session_id", string(msg.SessionID),
		"call_count", session.CallCount,
	)

	return &types.MsgSessionExecuteResponse{
		EncryptedResult: encryptedResult,
		GasUsed:         gasUsed,
	}, nil
}

// === ANUBIS Whisper Handlers ===

// ConfidentialTransfer handles MsgConfidentialTransfer
func (k msgServer) ConfidentialTransfer(goCtx context.Context, msg *types.MsgConfidentialTransfer) (*types.MsgConfidentialTransferResponse, error) {
	noteIDs, err := k.ProcessConfidentialTransfer(goCtx, msg)
	if err != nil {
		return nil, err
	}

	return &types.MsgConfidentialTransferResponse{
		NoteIDs: noteIDs,
	}, nil
}

// ShieldDeposit handles MsgShieldDeposit
func (k msgServer) ShieldDeposit(goCtx context.Context, msg *types.MsgShieldDeposit) (*types.MsgShieldDepositResponse, error) {
	noteID, err := k.ProcessShieldDeposit(goCtx, msg)
	if err != nil {
		return nil, err
	}

	return &types.MsgShieldDepositResponse{
		NoteID: noteID,
	}, nil
}

// UnshieldWithdraw handles MsgUnshieldWithdraw
func (k msgServer) UnshieldWithdraw(goCtx context.Context, msg *types.MsgUnshieldWithdraw) (*types.MsgUnshieldWithdrawResponse, error) {
	ctx := sdk.UnwrapSDKContext(goCtx)

	// Verify nullifier hasn't been spent
	if k.HasNullifier(goCtx, msg.Nullifier) {
		return nil, types.ErrNullifierAlreadySpent
	}

	// Verify ownership proof using SPARK implementation
	valid := k.VerifyOwnershipProof(goCtx, msg.Nullifier, msg.OwnershipProof)
	if !valid {
		return nil, types.ErrOwnershipProofFailed
	}

	// Mark nullifier as spent
	k.SetNullifier(goCtx, msg.Nullifier, ctx.BlockHeight())

	// Transfer funds from module to recipient
	recipientAddr, _ := sdk.AccAddressFromBech32(msg.Recipient)
	denom := msg.Denom
	if denom == "" {
		denom = "uaegis"
	}
	coins := sdk.NewCoins(sdk.NewInt64Coin(denom, int64(msg.Amount)))
	if err := k.bankKeeper.SendCoinsFromModuleToAccount(ctx, types.ModuleName, recipientAddr, coins); err != nil {
		return nil, err
	}

	// Emit event
	ctx.EventManager().EmitEvent(
		sdk.NewEvent(
			"unshield_withdraw",
			sdk.NewAttribute("recipient", msg.Recipient),
			sdk.NewAttribute("amount", fmt.Sprintf("%d", msg.Amount)),
		),
	)

	k.Logger(goCtx).Info("unshield withdrawal completed",
		"recipient", msg.Recipient,
		"amount", msg.Amount,
	)

	return &types.MsgUnshieldWithdrawResponse{}, nil
}

// === ANUBIS Eye Handlers ===

// CreateDisclosure handles MsgCreateDisclosure
func (k msgServer) CreateDisclosure(goCtx context.Context, msg *types.MsgCreateDisclosure) (*types.MsgCreateDisclosureResponse, error) {
	if err := k.ProcessCreateDisclosure(goCtx, msg); err != nil {
		return nil, err
	}

	// Generate disclosure ID
	disclosureID := k.generateDisclosureID(goCtx, msg.Owner, msg.RecipientPKHash)

	return &types.MsgCreateDisclosureResponse{
		DisclosureID: disclosureID,
	}, nil
}

// RevokeDisclosure handles MsgRevokeDisclosure
func (k msgServer) RevokeDisclosure(goCtx context.Context, msg *types.MsgRevokeDisclosure) (*types.MsgRevokeDisclosureResponse, error) {
	ctx := sdk.UnwrapSDKContext(goCtx)

	// Get disclosure
	disclosure, found := k.GetDisclosure(goCtx, msg.DisclosureID)
	if !found {
		return nil, types.ErrDisclosureNotFound
	}

	// Verify sender is owner
	if disclosure.Owner != msg.Owner {
		return nil, types.ErrUnauthorizedCaller
	}

	// Delete disclosure
	k.DeleteDisclosure(goCtx, msg.DisclosureID)

	// Emit event
	ctx.EventManager().EmitEvent(
		sdk.NewEvent(
			"revoke_disclosure",
			sdk.NewAttribute("disclosure_id", hex.EncodeToString(msg.DisclosureID)),
			sdk.NewAttribute("owner", msg.Owner),
		),
	)

	k.Logger(goCtx).Info("disclosure revoked",
		"disclosure_id", hex.EncodeToString(msg.DisclosureID),
	)

	return &types.MsgRevokeDisclosureResponse{}, nil
}

// === ANUBIS Prover Network Handlers ===

// RegisterProver handles MsgRegisterProver
func (k msgServer) RegisterProver(goCtx context.Context, msg *types.MsgRegisterProver) (*types.MsgRegisterProverResponse, error) {
	if err := k.Keeper.RegisterProver(goCtx, msg); err != nil {
		return nil, err
	}

	return &types.MsgRegisterProverResponse{}, nil
}

// UnbondProver handles MsgUnbondProver
func (k msgServer) UnbondProver(goCtx context.Context, msg *types.MsgUnbondProver) (*types.MsgUnbondProverResponse, error) {
	ctx := sdk.UnwrapSDKContext(goCtx)

	operatorAddr, err := sdk.AccAddressFromBech32(msg.Operator)
	if err != nil {
		return nil, types.Wrap(types.ErrInvalidAddress, "operator")
	}

	if err := k.Keeper.UnbondProver(ctx, operatorAddr); err != nil {
		return nil, err
	}

	return &types.MsgUnbondProverResponse{}, nil
}
