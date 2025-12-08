// Message types for x/anubis

package types

import (
	sdk "github.com/cosmos/cosmos-sdk/types"
)

// Proto interface implementations for Cosmos SDK v0.50 compatibility
func (m *MsgPrivateExecute) Reset()         { *m = MsgPrivateExecute{} }
func (m *MsgPrivateExecute) String() string { return m.Sender }
func (m *MsgPrivateExecute) ProtoMessage()  {}

func (m *MsgEnablePrivate) Reset()         { *m = MsgEnablePrivate{} }
func (m *MsgEnablePrivate) String() string { return m.Sender }
func (m *MsgEnablePrivate) ProtoMessage()  {}

func (m *MsgSubmitProof) Reset()         { *m = MsgSubmitProof{} }
func (m *MsgSubmitProof) String() string { return m.Sender }
func (m *MsgSubmitProof) ProtoMessage()  {}

func (m *MsgInitSession) Reset()         { *m = MsgInitSession{} }
func (m *MsgInitSession) String() string { return m.Sender }
func (m *MsgInitSession) ProtoMessage()  {}

func (m *MsgCloseSession) Reset()         { *m = MsgCloseSession{} }
func (m *MsgCloseSession) String() string { return m.Sender }
func (m *MsgCloseSession) ProtoMessage()  {}

func (m *MsgSessionExecute) Reset()         { *m = MsgSessionExecute{} }
func (m *MsgSessionExecute) String() string { return m.Sender }
func (m *MsgSessionExecute) ProtoMessage()  {}

// Message type constants
const (
	TypeMsgPrivateExecute     = "private_execute"
	TypeMsgEnablePrivate      = "enable_private"
	TypeMsgSubmitProof        = "submit_proof"
	TypeMsgInitSession        = "init_session"
	TypeMsgCloseSession       = "close_session"
	TypeMsgSessionExecute     = "session_execute"
)

var (
	_ sdk.Msg = &MsgPrivateExecute{}
	_ sdk.Msg = &MsgEnablePrivate{}
	_ sdk.Msg = &MsgSubmitProof{}
	_ sdk.Msg = &MsgInitSession{}
	_ sdk.Msg = &MsgCloseSession{}
	_ sdk.Msg = &MsgSessionExecute{}
)

// MsgPrivateExecute executes a contract privately
type MsgPrivateExecute struct {
	// Sender is the caller address
	Sender string `json:"sender"`

	// EncryptedCall contains the encrypted execution request
	EncryptedCall EncryptedCall `json:"encrypted_call"`

	// ExpectedGas is the expected gas for the call
	ExpectedGas uint64 `json:"expected_gas"`
}

// Route implements sdk.Msg
func (msg MsgPrivateExecute) Route() string { return RouterKey }

// Type implements sdk.Msg
func (msg MsgPrivateExecute) Type() string { return TypeMsgPrivateExecute }

// ValidateBasic implements sdk.Msg
func (msg MsgPrivateExecute) ValidateBasic() error {
	if _, err := sdk.AccAddressFromBech32(msg.Sender); err != nil {
		return Wrap(ErrInvalidAddress, "sender")
	}
	return ValidateEncryptedCall(msg.EncryptedCall)
}

// GetSigners implements sdk.Msg
func (msg MsgPrivateExecute) GetSigners() []sdk.AccAddress {
	sender, _ := sdk.AccAddressFromBech32(msg.Sender)
	return []sdk.AccAddress{sender}
}

// MsgEnablePrivate enables private execution for a contract
type MsgEnablePrivate struct {
	// Sender must be contract admin
	Sender string `json:"sender"`

	// Contract is the contract address
	Contract string `json:"contract"`

	// PublicKey is the ML-KEM-1024 public key
	PublicKey []byte `json:"public_key"`

	// EncryptionScheme is the encryption method
	EncryptionScheme string `json:"encryption_scheme"`

	// MaxPrivateCalls is the maximum concurrent private calls
	MaxPrivateCalls uint32 `json:"max_private_calls"`

	// AuthorizedCallers are addresses allowed to make private calls
	AuthorizedCallers []string `json:"authorized_callers,omitempty"`
}

// Route implements sdk.Msg
func (msg MsgEnablePrivate) Route() string { return RouterKey }

// Type implements sdk.Msg
func (msg MsgEnablePrivate) Type() string { return TypeMsgEnablePrivate }

// ValidateBasic implements sdk.Msg
func (msg MsgEnablePrivate) ValidateBasic() error {
	if _, err := sdk.AccAddressFromBech32(msg.Sender); err != nil {
		return Wrap(ErrInvalidAddress, "sender")
	}
	if _, err := sdk.AccAddressFromBech32(msg.Contract); err != nil {
		return Wrap(ErrInvalidAddress, "contract")
	}
	if len(msg.PublicKey) != 1568 {
		return ErrInvalidPublicKey
	}
	if msg.EncryptionScheme == "" {
		return ErrInvalidEncryption
	}
	return nil
}

// GetSigners implements sdk.Msg
func (msg MsgEnablePrivate) GetSigners() []sdk.AccAddress {
	sender, _ := sdk.AccAddressFromBech32(msg.Sender)
	return []sdk.AccAddress{sender}
}

// MsgSubmitProof submits a ZK proof for private execution
type MsgSubmitProof struct {
	// Sender is the proof submitter
	Sender string `json:"sender"`

	// Prover is the prover's address
	Prover string `json:"prover"`

	// RequestID references the private execution request
	RequestID []byte `json:"request_id"`

	// CallID references the encrypted call
	CallID []byte `json:"call_id"`

	// Proof is the ZK state transition proof
	Proof ZKStateProof `json:"proof"`

	// NewStateRoot is the new state root after execution
	NewStateRoot []byte `json:"new_state_root"`

	// PublicOutputs are the public outputs from execution
	PublicOutputs []byte `json:"public_outputs"`

	// EncryptedOutputs are the encrypted outputs for the caller
	EncryptedOutputs []byte `json:"encrypted_outputs"`

	// EncryptedState is the new encrypted contract state
	EncryptedState []byte `json:"encrypted_state"`

	// KEMCiphertext for decrypting the encrypted state
	KEMCiphertext []byte `json:"kem_ciphertext"`

	// GasUsed is the gas consumed during execution
	GasUsed uint64 `json:"gas_used"`

	// Signature is the prover's ML-DSA signature
	Signature []byte `json:"signature"`

	// EncryptedResult is the encrypted execution result
	EncryptedResult EncryptedResult `json:"encrypted_result"`
}

// Route implements sdk.Msg
func (msg MsgSubmitProof) Route() string { return RouterKey }

// Type implements sdk.Msg
func (msg MsgSubmitProof) Type() string { return TypeMsgSubmitProof }

// ValidateBasic implements sdk.Msg
func (msg MsgSubmitProof) ValidateBasic() error {
	if _, err := sdk.AccAddressFromBech32(msg.Sender); err != nil {
		return Wrap(ErrInvalidAddress, "sender")
	}
	if len(msg.CallID) != 32 {
		return ErrInvalidCallID
	}
	if len(msg.Proof.Proof) == 0 {
		return ErrZKProofInvalid
	}
	return nil
}

// GetSigners implements sdk.Msg
func (msg MsgSubmitProof) GetSigners() []sdk.AccAddress {
	sender, _ := sdk.AccAddressFromBech32(msg.Sender)
	return []sdk.AccAddress{sender}
}

// MsgInitSession initializes a private execution session
type MsgInitSession struct {
	// Sender is the session initiator
	Sender string `json:"sender"`

	// Contract is the target contract
	Contract string `json:"contract"`

	// KEMCiphertext is the encapsulated session key
	KEMCiphertext []byte `json:"kem_ciphertext"`

	// MaxCalls is the maximum calls in this session
	MaxCalls uint64 `json:"max_calls"`

	// Duration is the session duration in blocks
	Duration int64 `json:"duration"`
}

// Route implements sdk.Msg
func (msg MsgInitSession) Route() string { return RouterKey }

// Type implements sdk.Msg
func (msg MsgInitSession) Type() string { return TypeMsgInitSession }

// ValidateBasic implements sdk.Msg
func (msg MsgInitSession) ValidateBasic() error {
	if _, err := sdk.AccAddressFromBech32(msg.Sender); err != nil {
		return Wrap(ErrInvalidAddress, "sender")
	}
	if _, err := sdk.AccAddressFromBech32(msg.Contract); err != nil {
		return Wrap(ErrInvalidAddress, "contract")
	}
	if len(msg.KEMCiphertext) != 1568 {
		return ErrInvalidCiphertext
	}
	if msg.MaxCalls == 0 {
		return Wrap(ErrMaxCallsExceeded, "max calls must be positive")
	}
	return nil
}

// GetSigners implements sdk.Msg
func (msg MsgInitSession) GetSigners() []sdk.AccAddress {
	sender, _ := sdk.AccAddressFromBech32(msg.Sender)
	return []sdk.AccAddress{sender}
}

// MsgCloseSession closes a private execution session
type MsgCloseSession struct {
	// Sender must be session owner
	Sender string `json:"sender"`

	// SessionID is the session to close
	SessionID []byte `json:"session_id"`
}

// Route implements sdk.Msg
func (msg MsgCloseSession) Route() string { return RouterKey }

// Type implements sdk.Msg
func (msg MsgCloseSession) Type() string { return TypeMsgCloseSession }

// ValidateBasic implements sdk.Msg
func (msg MsgCloseSession) ValidateBasic() error {
	if _, err := sdk.AccAddressFromBech32(msg.Sender); err != nil {
		return Wrap(ErrInvalidAddress, "sender")
	}
	if len(msg.SessionID) != 32 {
		return Wrap(ErrInvalidCallID, "invalid session ID")
	}
	return nil
}

// GetSigners implements sdk.Msg
func (msg MsgCloseSession) GetSigners() []sdk.AccAddress {
	sender, _ := sdk.AccAddressFromBech32(msg.Sender)
	return []sdk.AccAddress{sender}
}

// MsgSessionExecute executes within a session
type MsgSessionExecute struct {
	// Sender must be session owner
	Sender string `json:"sender"`

	// SessionID is the session to use
	SessionID []byte `json:"session_id"`

	// EncryptedFunction is the encrypted function selector
	EncryptedFunction []byte `json:"encrypted_function"`

	// EncryptedArgs is the encrypted arguments
	EncryptedArgs []byte `json:"encrypted_args"`

	// Nonce is the call nonce within the session
	Nonce uint64 `json:"nonce"`
}

// Route implements sdk.Msg
func (msg MsgSessionExecute) Route() string { return RouterKey }

// Type implements sdk.Msg
func (msg MsgSessionExecute) Type() string { return TypeMsgSessionExecute }

// ValidateBasic implements sdk.Msg
func (msg MsgSessionExecute) ValidateBasic() error {
	if _, err := sdk.AccAddressFromBech32(msg.Sender); err != nil {
		return Wrap(ErrInvalidAddress, "sender")
	}
	if len(msg.SessionID) != 32 {
		return Wrap(ErrInvalidCallID, "invalid session ID")
	}
	return nil
}

// GetSigners implements sdk.Msg
func (msg MsgSessionExecute) GetSigners() []sdk.AccAddress {
	sender, _ := sdk.AccAddressFromBech32(msg.Sender)
	return []sdk.AccAddress{sender}
}

// Response types
type MsgPrivateExecuteResponse struct {
	CallID []byte `json:"call_id"`
}

type MsgEnablePrivateResponse struct{}

type MsgSubmitProofResponse struct {
	Verified bool `json:"verified"`
}

type MsgInitSessionResponse struct {
	SessionID []byte `json:"session_id"`
	ExpiresAt int64  `json:"expires_at"`
}

type MsgCloseSessionResponse struct{}

type MsgSessionExecuteResponse struct {
	EncryptedResult []byte `json:"encrypted_result"`
	GasUsed         uint64 `json:"gas_used"`
}

// === ANUBIS Whisper Messages ===

// Message type constants for ANUBIS
const (
	TypeMsgConfidentialTransfer = "confidential_transfer"
	TypeMsgShieldDeposit        = "shield_deposit"
	TypeMsgUnshieldWithdraw     = "unshield_withdraw"
	TypeMsgCreateDisclosure     = "create_disclosure"
	TypeMsgRevokeDisclosure     = "revoke_disclosure"
	TypeMsgRegisterProver       = "register_prover"
	TypeMsgUnbondProver         = "unbond_prover"
)

var (
	_ sdk.Msg = &MsgConfidentialTransfer{}
	_ sdk.Msg = &MsgShieldDeposit{}
	_ sdk.Msg = &MsgUnshieldWithdraw{}
	_ sdk.Msg = &MsgCreateDisclosure{}
	_ sdk.Msg = &MsgRevokeDisclosure{}
	_ sdk.Msg = &MsgRegisterProver{}
	_ sdk.Msg = &MsgUnbondProver{}
)

// MsgShieldDeposit shields public funds into a confidential note
type MsgShieldDeposit struct {
	Sender       string `json:"sender"`
	Amount       uint64 `json:"amount"`
	RecipientPK  []byte `json:"recipient_pk"`  // ML-KEM public key
	Blinding     []byte `json:"blinding"`      // Blinding factor for commitment
	Denom        string `json:"denom"`
}

func (m *MsgShieldDeposit) Reset()         { *m = MsgShieldDeposit{} }
func (m *MsgShieldDeposit) String() string { return m.Sender }
func (m *MsgShieldDeposit) ProtoMessage()  {}
func (msg MsgShieldDeposit) Route() string { return RouterKey }
func (msg MsgShieldDeposit) Type() string  { return TypeMsgShieldDeposit }

func (msg MsgShieldDeposit) ValidateBasic() error {
	if _, err := sdk.AccAddressFromBech32(msg.Sender); err != nil {
		return Wrap(ErrInvalidAddress, "sender")
	}
	if msg.Amount == 0 {
		return Wrap(ErrInvalidNote, "amount must be positive")
	}
	if len(msg.RecipientPK) != 1568 {
		return ErrInvalidPublicKey
	}
	if len(msg.Blinding) != 32 {
		return Wrap(ErrInvalidNote, "invalid blinding factor")
	}
	return nil
}

func (msg MsgShieldDeposit) GetSigners() []sdk.AccAddress {
	sender, _ := sdk.AccAddressFromBech32(msg.Sender)
	return []sdk.AccAddress{sender}
}

// MsgUnshieldWithdraw withdraws from confidential to public
type MsgUnshieldWithdraw struct {
	Sender         string   `json:"sender"`
	Nullifier      []byte   `json:"nullifier"`
	Amount         uint64   `json:"amount"`
	Recipient      string   `json:"recipient"`
	OwnershipProof []byte   `json:"ownership_proof"`
	Denom          string   `json:"denom"`
}

func (m *MsgUnshieldWithdraw) Reset()         { *m = MsgUnshieldWithdraw{} }
func (m *MsgUnshieldWithdraw) String() string { return m.Sender }
func (m *MsgUnshieldWithdraw) ProtoMessage()  {}
func (msg MsgUnshieldWithdraw) Route() string { return RouterKey }
func (msg MsgUnshieldWithdraw) Type() string  { return TypeMsgUnshieldWithdraw }

func (msg MsgUnshieldWithdraw) ValidateBasic() error {
	if _, err := sdk.AccAddressFromBech32(msg.Sender); err != nil {
		return Wrap(ErrInvalidAddress, "sender")
	}
	if _, err := sdk.AccAddressFromBech32(msg.Recipient); err != nil {
		return Wrap(ErrInvalidAddress, "recipient")
	}
	if len(msg.Nullifier) != 32 {
		return ErrInvalidNullifier
	}
	if len(msg.OwnershipProof) == 0 {
		return ErrOwnershipProofFailed
	}
	return nil
}

func (msg MsgUnshieldWithdraw) GetSigners() []sdk.AccAddress {
	sender, _ := sdk.AccAddressFromBech32(msg.Sender)
	return []sdk.AccAddress{sender}
}

// Route/Type/ValidateBasic/GetSigners for MsgConfidentialTransfer
func (msg MsgConfidentialTransfer) Route() string { return RouterKey }
func (msg MsgConfidentialTransfer) Type() string  { return TypeMsgConfidentialTransfer }

func (msg MsgConfidentialTransfer) ValidateBasic() error {
	if _, err := sdk.AccAddressFromBech32(msg.Sender); err != nil {
		return Wrap(ErrInvalidAddress, "sender")
	}
	if len(msg.InputNullifiers) == 0 {
		return Wrap(ErrInvalidNullifier, "no input nullifiers")
	}
	if len(msg.OutputNotes) == 0 {
		return Wrap(ErrInvalidNote, "no output notes")
	}
	if len(msg.BalanceProof) == 0 {
		return ErrBalanceProofFailed
	}
	return nil
}

func (msg MsgConfidentialTransfer) GetSigners() []sdk.AccAddress {
	sender, _ := sdk.AccAddressFromBech32(msg.Sender)
	return []sdk.AccAddress{sender}
}

// === ANUBIS Eye Messages ===

// MsgRevokeDisclosure revokes an existing disclosure
type MsgRevokeDisclosure struct {
	Owner        string `json:"owner"`
	DisclosureID []byte `json:"disclosure_id"`
	Signature    []byte `json:"signature"`
}

func (m *MsgRevokeDisclosure) Reset()         { *m = MsgRevokeDisclosure{} }
func (m *MsgRevokeDisclosure) String() string { return m.Owner }
func (m *MsgRevokeDisclosure) ProtoMessage()  {}
func (msg MsgRevokeDisclosure) Route() string { return RouterKey }
func (msg MsgRevokeDisclosure) Type() string  { return TypeMsgRevokeDisclosure }

func (msg MsgRevokeDisclosure) ValidateBasic() error {
	if _, err := sdk.AccAddressFromBech32(msg.Owner); err != nil {
		return Wrap(ErrInvalidAddress, "owner")
	}
	if len(msg.DisclosureID) != 32 {
		return Wrap(ErrDisclosureNotFound, "invalid disclosure ID")
	}
	return nil
}

func (msg MsgRevokeDisclosure) GetSigners() []sdk.AccAddress {
	sender, _ := sdk.AccAddressFromBech32(msg.Owner)
	return []sdk.AccAddress{sender}
}

// Route/Type/ValidateBasic/GetSigners for MsgCreateDisclosure
func (msg MsgCreateDisclosure) Route() string { return RouterKey }
func (msg MsgCreateDisclosure) Type() string  { return TypeMsgCreateDisclosure }

func (msg MsgCreateDisclosure) ValidateBasic() error {
	if _, err := sdk.AccAddressFromBech32(msg.Owner); err != nil {
		return Wrap(ErrInvalidAddress, "owner")
	}
	if len(msg.OwnerPK) != 2592 { // ML-DSA-87 public key size
		return ErrInvalidPublicKey
	}
	if len(msg.RecipientPKHash) != 32 {
		return Wrap(ErrInvalidAddress, "invalid recipient hash")
	}
	if len(msg.Proof) == 0 {
		return ErrInvalidDisclosureProof
	}
	return nil
}

func (msg MsgCreateDisclosure) GetSigners() []sdk.AccAddress {
	sender, _ := sdk.AccAddressFromBech32(msg.Owner)
	return []sdk.AccAddress{sender}
}

// === ANUBIS Prover Messages ===

// MsgUnbondProver initiates unbonding for a prover
type MsgUnbondProver struct {
	Operator string `json:"operator"`
}

func (m *MsgUnbondProver) Reset()         { *m = MsgUnbondProver{} }
func (m *MsgUnbondProver) String() string { return m.Operator }
func (m *MsgUnbondProver) ProtoMessage()  {}
func (msg MsgUnbondProver) Route() string { return RouterKey }
func (msg MsgUnbondProver) Type() string  { return TypeMsgUnbondProver }

func (msg MsgUnbondProver) ValidateBasic() error {
	if _, err := sdk.AccAddressFromBech32(msg.Operator); err != nil {
		return Wrap(ErrInvalidAddress, "operator")
	}
	return nil
}

func (msg MsgUnbondProver) GetSigners() []sdk.AccAddress {
	sender, _ := sdk.AccAddressFromBech32(msg.Operator)
	return []sdk.AccAddress{sender}
}

// Route/Type/ValidateBasic/GetSigners for MsgRegisterProver
func (msg MsgRegisterProver) Route() string { return RouterKey }
func (msg MsgRegisterProver) Type() string  { return TypeMsgRegisterProver }

func (msg MsgRegisterProver) ValidateBasic() error {
	if _, err := sdk.AccAddressFromBech32(msg.Operator); err != nil {
		return Wrap(ErrInvalidAddress, "operator")
	}
	if len(msg.ProverPK) != 2592 { // ML-DSA-87 public key size
		return ErrInvalidPublicKey
	}
	if msg.StakeAmount == 0 {
		return ErrInsufficientStake
	}
	if msg.Commission > 10000 { // Max 100% in basis points
		return Wrap(ErrInvalidAddress, "commission exceeds maximum")
	}
	return nil
}

func (msg MsgRegisterProver) GetSigners() []sdk.AccAddress {
	sender, _ := sdk.AccAddressFromBech32(msg.Operator)
	return []sdk.AccAddress{sender}
}

// === Response types for ANUBIS messages ===

type MsgConfidentialTransferResponse struct {
	NoteIDs [][]byte `json:"note_ids"`
}

type MsgShieldDepositResponse struct {
	NoteID []byte `json:"note_id"`
}

type MsgUnshieldWithdrawResponse struct{}

type MsgCreateDisclosureResponse struct {
	DisclosureID []byte `json:"disclosure_id"`
}

type MsgRevokeDisclosureResponse struct{}

type MsgRegisterProverResponse struct{}

type MsgUnbondProverResponse struct{}
