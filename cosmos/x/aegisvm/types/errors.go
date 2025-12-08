// Error definitions for x/aegisvm

package types

import (
	sdkerrors "cosmossdk.io/errors"
)

// Module sentinel errors
var (
	ErrInvalidProfile     = sdkerrors.Register(ModuleName, 1, "invalid security profile: must be PQ-L5-Strict")
	ErrInvalidBytecode    = sdkerrors.Register(ModuleName, 2, "invalid bytecode")
	ErrMissingProofHash   = sdkerrors.Register(ModuleName, 3, "missing proof hash")
	ErrUnapprovedCode     = sdkerrors.Register(ModuleName, 4, "code hash not approved by governance")
	ErrContractNotFound   = sdkerrors.Register(ModuleName, 5, "contract not found")
	ErrContractExists     = sdkerrors.Register(ModuleName, 6, "contract already exists at address")
	ErrExecutionFailed    = sdkerrors.Register(ModuleName, 7, "contract execution failed")
	ErrOutOfGas           = sdkerrors.Register(ModuleName, 8, "out of gas")
	ErrInvalidFunction    = sdkerrors.Register(ModuleName, 9, "invalid function selector")
	ErrInvalidArguments   = sdkerrors.Register(ModuleName, 10, "invalid function arguments")
	ErrStorageAccess      = sdkerrors.Register(ModuleName, 11, "storage access violation")
	ErrStackOverflow      = sdkerrors.Register(ModuleName, 12, "call stack overflow")
	ErrInvalidOpcode      = sdkerrors.Register(ModuleName, 13, "invalid opcode")
	ErrContractReverted   = sdkerrors.Register(ModuleName, 14, "contract execution reverted")
	ErrInsufficientFunds  = sdkerrors.Register(ModuleName, 15, "insufficient funds for transfer")
	ErrUnauthorized       = sdkerrors.Register(ModuleName, 16, "unauthorized operation")
	ErrInvalidState       = sdkerrors.Register(ModuleName, 17, "invalid contract state")
	ErrCodeTooLarge       = sdkerrors.Register(ModuleName, 18, "bytecode exceeds maximum size")
	ErrInvalidSignature   = sdkerrors.Register(ModuleName, 19, "invalid ML-DSA signature")
	ErrInvalidAddress     = sdkerrors.Register(ModuleName, 20, "invalid address format")
)

// Wrap wraps an error with additional context
func Wrap(err error, msg string) error {
	return sdkerrors.Wrap(err, msg)
}
