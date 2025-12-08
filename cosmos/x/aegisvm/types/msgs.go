// Message types for x/aegisvm

package types

import (
	sdk "github.com/cosmos/cosmos-sdk/types"
)

// Proto interface implementations for Cosmos SDK v0.50 compatibility
func (m *MsgDeployContract) Reset()         { *m = MsgDeployContract{} }
func (m *MsgDeployContract) String() string { return m.Sender }
func (m *MsgDeployContract) ProtoMessage()  {}

func (m *MsgExecuteContract) Reset()         { *m = MsgExecuteContract{} }
func (m *MsgExecuteContract) String() string { return m.Sender }
func (m *MsgExecuteContract) ProtoMessage()  {}

func (m *MsgMigrateContract) Reset()         { *m = MsgMigrateContract{} }
func (m *MsgMigrateContract) String() string { return m.Sender }
func (m *MsgMigrateContract) ProtoMessage()  {}

func (m *MsgUpdateAdmin) Reset()         { *m = MsgUpdateAdmin{} }
func (m *MsgUpdateAdmin) String() string { return m.Sender }
func (m *MsgUpdateAdmin) ProtoMessage()  {}

// Message type constants
const (
	TypeMsgDeployContract  = "deploy_contract"
	TypeMsgExecuteContract = "execute_contract"
	TypeMsgMigrateContract = "migrate_contract"
	TypeMsgUpdateAdmin     = "update_admin"
)

var (
	_ sdk.Msg = &MsgDeployContract{}
	_ sdk.Msg = &MsgExecuteContract{}
	_ sdk.Msg = &MsgMigrateContract{}
	_ sdk.Msg = &MsgUpdateAdmin{}
)

// MsgDeployContract deploys a new smart contract
type MsgDeployContract struct {
	// Sender is the deployer address (bech32)
	Sender string `json:"sender"`

	// Bytecode is the compiled AegisVM bytecode
	Bytecode []byte `json:"bytecode"`

	// InitArgs are the initialization arguments
	InitArgs []byte `json:"init_args,omitempty"`

	// Profile must be "PQ-L5-Strict"
	Profile string `json:"profile"`

	// ProofHash is the SHA3 of SPARK proof artifacts
	ProofHash []byte `json:"proof_hash"`

	// KATHash is the SHA3 of Known Answer Test vectors
	KATHash []byte `json:"kat_hash,omitempty"`

	// Admin is an optional admin address for contract upgrades
	Admin string `json:"admin,omitempty"`

	// Label is a human-readable label for the contract
	Label string `json:"label,omitempty"`
}

// Route implements sdk.Msg
func (msg MsgDeployContract) Route() string { return RouterKey }

// Type implements sdk.Msg
func (msg MsgDeployContract) Type() string { return TypeMsgDeployContract }

// ValidateBasic implements sdk.Msg
func (msg MsgDeployContract) ValidateBasic() error {
	if _, err := sdk.AccAddressFromBech32(msg.Sender); err != nil {
		return Wrap(ErrInvalidAddress, "sender")
	}
	if len(msg.Bytecode) == 0 {
		return ErrInvalidBytecode
	}
	if msg.Profile != ProfilePQL5Strict {
		return ErrInvalidProfile
	}
	if len(msg.ProofHash) != 32 {
		return ErrMissingProofHash
	}
	return nil
}

// GetSigners implements sdk.Msg
func (msg MsgDeployContract) GetSigners() []sdk.AccAddress {
	sender, _ := sdk.AccAddressFromBech32(msg.Sender)
	return []sdk.AccAddress{sender}
}

// MsgExecuteContract executes a contract function
type MsgExecuteContract struct {
	// Sender is the caller address (bech32)
	Sender string `json:"sender"`

	// Contract is the contract address (bech32)
	Contract string `json:"contract"`

	// Function is the function selector (4 bytes)
	Function []byte `json:"function"`

	// Args are the function arguments
	Args []byte `json:"args,omitempty"`

	// Funds are coins sent with the call
	Funds sdk.Coins `json:"funds,omitempty"`
}

// Route implements sdk.Msg
func (msg MsgExecuteContract) Route() string { return RouterKey }

// Type implements sdk.Msg
func (msg MsgExecuteContract) Type() string { return TypeMsgExecuteContract }

// ValidateBasic implements sdk.Msg
func (msg MsgExecuteContract) ValidateBasic() error {
	if _, err := sdk.AccAddressFromBech32(msg.Sender); err != nil {
		return Wrap(ErrInvalidAddress, "sender")
	}
	if _, err := sdk.AccAddressFromBech32(msg.Contract); err != nil {
		return Wrap(ErrInvalidAddress, "contract")
	}
	if len(msg.Function) != 4 {
		return ErrInvalidFunction
	}
	if !msg.Funds.IsValid() {
		return ErrInsufficientFunds
	}
	return nil
}

// GetSigners implements sdk.Msg
func (msg MsgExecuteContract) GetSigners() []sdk.AccAddress {
	sender, _ := sdk.AccAddressFromBech32(msg.Sender)
	return []sdk.AccAddress{sender}
}

// MsgMigrateContract migrates a contract to new bytecode
type MsgMigrateContract struct {
	// Sender must be the contract admin
	Sender string `json:"sender"`

	// Contract is the contract address to migrate
	Contract string `json:"contract"`

	// NewBytecode is the new contract bytecode
	NewBytecode []byte `json:"new_bytecode"`

	// NewProofHash is the SHA3 of new SPARK proofs
	NewProofHash []byte `json:"new_proof_hash"`

	// MigrateArgs are passed to the migrate function
	MigrateArgs []byte `json:"migrate_args,omitempty"`
}

// Route implements sdk.Msg
func (msg MsgMigrateContract) Route() string { return RouterKey }

// Type implements sdk.Msg
func (msg MsgMigrateContract) Type() string { return TypeMsgMigrateContract }

// ValidateBasic implements sdk.Msg
func (msg MsgMigrateContract) ValidateBasic() error {
	if _, err := sdk.AccAddressFromBech32(msg.Sender); err != nil {
		return Wrap(ErrInvalidAddress, "sender")
	}
	if _, err := sdk.AccAddressFromBech32(msg.Contract); err != nil {
		return Wrap(ErrInvalidAddress, "contract")
	}
	if len(msg.NewBytecode) == 0 {
		return ErrInvalidBytecode
	}
	if len(msg.NewProofHash) != 32 {
		return ErrMissingProofHash
	}
	return nil
}

// GetSigners implements sdk.Msg
func (msg MsgMigrateContract) GetSigners() []sdk.AccAddress {
	sender, _ := sdk.AccAddressFromBech32(msg.Sender)
	return []sdk.AccAddress{sender}
}

// MsgUpdateAdmin updates the contract admin
type MsgUpdateAdmin struct {
	// Sender must be current admin
	Sender string `json:"sender"`

	// Contract is the contract address
	Contract string `json:"contract"`

	// NewAdmin is the new admin address (empty to remove admin)
	NewAdmin string `json:"new_admin,omitempty"`
}

// Route implements sdk.Msg
func (msg MsgUpdateAdmin) Route() string { return RouterKey }

// Type implements sdk.Msg
func (msg MsgUpdateAdmin) Type() string { return TypeMsgUpdateAdmin }

// ValidateBasic implements sdk.Msg
func (msg MsgUpdateAdmin) ValidateBasic() error {
	if _, err := sdk.AccAddressFromBech32(msg.Sender); err != nil {
		return Wrap(ErrInvalidAddress, "sender")
	}
	if _, err := sdk.AccAddressFromBech32(msg.Contract); err != nil {
		return Wrap(ErrInvalidAddress, "contract")
	}
	if msg.NewAdmin != "" {
		if _, err := sdk.AccAddressFromBech32(msg.NewAdmin); err != nil {
			return Wrap(ErrInvalidAddress, "new_admin")
		}
	}
	return nil
}

// GetSigners implements sdk.Msg
func (msg MsgUpdateAdmin) GetSigners() []sdk.AccAddress {
	sender, _ := sdk.AccAddressFromBech32(msg.Sender)
	return []sdk.AccAddress{sender}
}
