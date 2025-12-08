// Error definitions for x/anubis

package types

import (
	sdkerrors "cosmossdk.io/errors"
)

// Module sentinel errors
var (
	ErrPrivateExecDisabled    = sdkerrors.Register(ModuleName, 1, "private execution is disabled")
	ErrInvalidEncryption      = sdkerrors.Register(ModuleName, 2, "invalid encryption scheme")
	ErrDecryptionFailed       = sdkerrors.Register(ModuleName, 3, "decryption failed")
	ErrInvalidCiphertext      = sdkerrors.Register(ModuleName, 4, "invalid ciphertext")
	ErrInvalidPublicKey       = sdkerrors.Register(ModuleName, 5, "invalid public key")
	ErrInvalidSignature       = sdkerrors.Register(ModuleName, 6, "invalid ML-DSA signature")
	ErrInvalidCallID          = sdkerrors.Register(ModuleName, 7, "invalid call ID")
	ErrCallNotFound           = sdkerrors.Register(ModuleName, 8, "encrypted call not found")
	ErrSessionExpired         = sdkerrors.Register(ModuleName, 9, "session has expired")
	ErrSessionLimitReached    = sdkerrors.Register(ModuleName, 10, "session call limit reached")
	ErrUnauthorizedCaller     = sdkerrors.Register(ModuleName, 11, "caller not authorized for private calls")
	ErrContractNotPrivate     = sdkerrors.Register(ModuleName, 12, "contract does not support private execution")
	ErrZKProofInvalid         = sdkerrors.Register(ModuleName, 13, "ZK proof verification failed")
	ErrZKProofNotFound        = sdkerrors.Register(ModuleName, 14, "ZK proof not found")
	ErrStateRootMismatch      = sdkerrors.Register(ModuleName, 15, "state root mismatch")
	ErrKEMDecapsulationFailed = sdkerrors.Register(ModuleName, 16, "ML-KEM decapsulation failed")
	ErrFHEOperationFailed     = sdkerrors.Register(ModuleName, 17, "FHE operation failed")
	ErrMaxCallsExceeded       = sdkerrors.Register(ModuleName, 18, "maximum private calls exceeded")
	ErrInvalidAddress         = sdkerrors.Register(ModuleName, 19, "invalid address format")
	ErrReplayDetected         = sdkerrors.Register(ModuleName, 20, "replay attack detected")

	// ANUBIS Whisper (Confidential Transactions)
	ErrNullifierAlreadySpent  = sdkerrors.Register(ModuleName, 21, "nullifier already spent")
	ErrInvalidNullifier       = sdkerrors.Register(ModuleName, 22, "invalid nullifier")
	ErrInvalidNote            = sdkerrors.Register(ModuleName, 23, "invalid note")
	ErrRangeProofFailed       = sdkerrors.Register(ModuleName, 24, "range proof verification failed")
	ErrBalanceProofFailed     = sdkerrors.Register(ModuleName, 25, "balance proof verification failed")
	ErrOwnershipProofFailed   = sdkerrors.Register(ModuleName, 26, "ownership proof verification failed")
	ErrNoteNotFound           = sdkerrors.Register(ModuleName, 27, "note not found")
	ErrDoubleSpend            = sdkerrors.Register(ModuleName, 43, "double spend detected")
	ErrInvalidOwnershipProof  = sdkerrors.Register(ModuleName, 44, "invalid ownership proof")
	ErrInvalidRangeProof      = sdkerrors.Register(ModuleName, 45, "invalid range proof")
	ErrNullifierNotFound      = sdkerrors.Register(ModuleName, 46, "nullifier not found")
	ErrInvalidBalanceProof    = sdkerrors.Register(ModuleName, 47, "invalid balance proof")

	// ANUBIS Shield (Encrypted State)
	ErrEncryptionFailed       = sdkerrors.Register(ModuleName, 48, "encryption failed")
	ErrInvalidSecretKey       = sdkerrors.Register(ModuleName, 49, "invalid secret key")

	// ANUBIS Eye (Selective Disclosure)
	ErrInvalidDisclosureProof = sdkerrors.Register(ModuleName, 28, "disclosure proof verification failed")
	ErrDisclosureNotFound     = sdkerrors.Register(ModuleName, 29, "disclosure not found")
	ErrDisclosureExpired      = sdkerrors.Register(ModuleName, 30, "disclosure has expired")

	// ANUBIS Gate (Private Execution)
	ErrContractNotFound       = sdkerrors.Register(ModuleName, 31, "contract not found")
	ErrPrivateNotEnabled      = sdkerrors.Register(ModuleName, 32, "private execution not enabled for contract")
	ErrRequestNotFound        = sdkerrors.Register(ModuleName, 33, "private execution request not found")
	ErrRequestNotPending      = sdkerrors.Register(ModuleName, 34, "request is not pending")
	ErrRequestExpired         = sdkerrors.Register(ModuleName, 35, "private execution request expired")
	ErrInvalidInputProof      = sdkerrors.Register(ModuleName, 36, "invalid input proof")
	ErrInvalidExecutionProof  = sdkerrors.Register(ModuleName, 37, "invalid execution proof")

	// ANUBIS Prover Network
	ErrProverAlreadyExists    = sdkerrors.Register(ModuleName, 38, "prover already registered")
	ErrProverNotFound         = sdkerrors.Register(ModuleName, 39, "prover not found")
	ErrProverNotActive        = sdkerrors.Register(ModuleName, 40, "prover is not active")
	ErrInsufficientStake      = sdkerrors.Register(ModuleName, 41, "insufficient prover stake")
	ErrInvalidProver          = sdkerrors.Register(ModuleName, 42, "invalid or inactive prover")
)

// Wrap wraps an error with additional context
func Wrap(err error, msg string) error {
	return sdkerrors.Wrap(err, msg)
}
