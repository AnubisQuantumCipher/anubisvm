// Package scarab provides SCARAB v2.0 proof aggregation functionality.
//
// SCARAB implements efficient signature aggregation and verification for
// the AegisVM blockchain, enabling massive block compression while
// maintaining post-quantum security.
//
// Components:
//   - KHNUM: TX signature aggregation (4096 sigs -> 4KB proof)
//   - SEBEK: Threshold signature scheme (t-of-n ML-DSA-87)
//   - MAAT: Hierarchical proof aggregation
//   - TEFNUT: Light client proofs
//   - SEKHMET: SIMD acceleration
//   - KHEPRI: Native SPARK contract system

package scarab

/*
#cgo CFLAGS: -I${SRCDIR}/../../../core/include
#cgo LDFLAGS: -L${SRCDIR}/../../../core/lib -laegisvm

#include "aegisvm.h"
#include <stdlib.h>
*/
import "C"

import (
	"errors"
	"unsafe"
)

// Errors
var (
	ErrInvalidInput      = errors.New("invalid input parameters")
	ErrAggregationFailed = errors.New("signature aggregation failed")
	ErrVerificationFailed = errors.New("proof verification failed")
	ErrThresholdNotMet   = errors.New("threshold not met")
	ErrProverFailed      = errors.New("prover operation failed")
)

// Constants
const (
	// KHNUM
	MaxBatchSize       = 4096
	AggregatedSigSize  = 4096
	MLDSASignatureSize = 4627
	MLDSAPublicKeySize = 2592

	// SEBEK
	MaxCommitteeSize    = 67
	DefaultThreshold    = 45
	ShareSize           = 256
	CombinedSigSize     = 4800

	// SIMD backends
	SIMDScalar  = 0
	SIMDAVX2    = 1
	SIMDAVX512  = 2
	SIMDNEON    = 3
	SIMDSVE     = 4

	// Certification levels
	CertBronze   = 0
	CertSilver   = 1
	CertGold     = 2
	CertPlatinum = 3
)

// ============================================================================
// KHNUM - TX Signature Aggregation
// ============================================================================

// AggregateSignatures aggregates multiple ML-DSA-87 signatures into a single proof.
// This enables massive block compression (4096 sigs -> 4KB proof).
func AggregateSignatures(
	signatures [][MLDSASignatureSize]byte,
	publicKeys [][MLDSAPublicKeySize]byte,
	messages [][32]byte,
) ([]byte, error) {
	if len(signatures) == 0 || len(signatures) > MaxBatchSize {
		return nil, ErrInvalidInput
	}
	if len(signatures) != len(publicKeys) || len(signatures) != len(messages) {
		return nil, ErrInvalidInput
	}

	count := len(signatures)
	proof := make([]byte, 32768) // Max aggregated proof size
	var proofLen C.size_t

	result := C.aegis_khnum_aggregate_signatures(
		(*C.uint8_t)(unsafe.Pointer(&proof[0])),
		&proofLen,
		(*[MLDSASignatureSize]C.uint8_t)(unsafe.Pointer(&signatures[0])),
		(*[MLDSAPublicKeySize]C.uint8_t)(unsafe.Pointer(&publicKeys[0])),
		(*[32]C.uint8_t)(unsafe.Pointer(&messages[0])),
		C.size_t(count),
	)

	if result != 0 {
		return nil, ErrAggregationFailed
	}

	return proof[:proofLen], nil
}

// VerifyAggregatedSignatures verifies an aggregated signature proof.
func VerifyAggregatedSignatures(
	proof []byte,
	publicKeys [][MLDSAPublicKeySize]byte,
	messages [][32]byte,
) (bool, error) {
	if len(proof) == 0 || len(publicKeys) == 0 {
		return false, ErrInvalidInput
	}
	if len(publicKeys) != len(messages) {
		return false, ErrInvalidInput
	}

	var valid C.bool
	count := len(publicKeys)

	result := C.aegis_khnum_verify_aggregated(
		(*C.uint8_t)(unsafe.Pointer(&proof[0])),
		C.size_t(len(proof)),
		(*[MLDSAPublicKeySize]C.uint8_t)(unsafe.Pointer(&publicKeys[0])),
		(*[32]C.uint8_t)(unsafe.Pointer(&messages[0])),
		C.size_t(count),
		&valid,
	)

	if result != 0 {
		return false, ErrVerificationFailed
	}

	return bool(valid), nil
}

// ============================================================================
// SEBEK - Threshold Signatures
// ============================================================================

// KeyShare represents a threshold key share
type KeyShare struct {
	Share      [ShareSize]byte
	Index      uint32
	Commitment [32]byte
}

// SignPartial generates a partial signature using a key share.
func SignPartial(share *KeyShare, message []byte) ([]byte, error) {
	if share == nil || len(message) == 0 {
		return nil, ErrInvalidInput
	}

	partialSig := make([]byte, ShareSize)
	var sigLen C.size_t

	// Create C share struct
	type cShare struct {
		share      [ShareSize]C.uint8_t
		index      C.uint32_t
		commitment [32]C.uint8_t
	}
	var cs cShare
	for i := 0; i < ShareSize; i++ {
		cs.share[i] = C.uint8_t(share.Share[i])
	}
	cs.index = C.uint32_t(share.Index)
	for i := 0; i < 32; i++ {
		cs.commitment[i] = C.uint8_t(share.Commitment[i])
	}

	result := C.aegis_sebek_sign_partial(
		(*C.uint8_t)(unsafe.Pointer(&partialSig[0])),
		&sigLen,
		(*C.aegis_sebek_share_t)(unsafe.Pointer(&cs)),
		(*C.uint8_t)(unsafe.Pointer(&message[0])),
		C.size_t(len(message)),
	)

	if result != 0 {
		return nil, ErrThresholdNotMet
	}

	return partialSig[:sigLen], nil
}

// CombinePartialSignatures combines partial signatures into a threshold signature.
func CombinePartialSignatures(partials [][]byte, indices []uint32) ([]byte, error) {
	if len(partials) == 0 || len(partials) != len(indices) {
		return nil, ErrInvalidInput
	}

	// Flatten partials to array
	partialsFlat := make([][ShareSize]byte, len(partials))
	for i, p := range partials {
		copy(partialsFlat[i][:], p)
	}

	combined := make([]byte, CombinedSigSize)
	var sigLen C.size_t

	result := C.aegis_sebek_combine_signatures(
		(*C.uint8_t)(unsafe.Pointer(&combined[0])),
		&sigLen,
		(*[ShareSize]C.uint8_t)(unsafe.Pointer(&partialsFlat[0])),
		(*C.uint32_t)(unsafe.Pointer(&indices[0])),
		C.size_t(len(partials)),
	)

	if result != 0 {
		return nil, ErrThresholdNotMet
	}

	return combined[:sigLen], nil
}

// VerifyThresholdSignature verifies a threshold signature against a combined public key.
func VerifyThresholdSignature(signature []byte, message []byte, combinedPK [32]byte) (bool, error) {
	if len(signature) == 0 || len(message) == 0 {
		return false, ErrInvalidInput
	}

	var valid C.bool

	result := C.aegis_sebek_verify(
		(*C.uint8_t)(unsafe.Pointer(&signature[0])),
		C.size_t(len(signature)),
		(*C.uint8_t)(unsafe.Pointer(&message[0])),
		C.size_t(len(message)),
		(*C.uint8_t)(unsafe.Pointer(&combinedPK[0])),
		&valid,
	)

	if result != 0 {
		return false, ErrVerificationFailed
	}

	return bool(valid), nil
}

// ============================================================================
// MAAT - Hierarchical Aggregation
// ============================================================================

// BlockProof represents a MAAT block proof
type BlockProof struct {
	BatchCommits   [16][32]byte
	AggregatedProof []byte
	ProofLen       uint64
	BlockHeight    uint64
}

// EpochProof represents a MAAT epoch proof for light clients
type EpochProof struct {
	BlockRoots  [100][32]byte
	EpochRoot   [32]byte
	StateRoot   [32]byte
	EpochNumber uint64
	Proof       []byte
}

// GenerateBlockProof generates a MAAT block proof for a batch of transactions.
func GenerateBlockProof(
	signatures [][MLDSASignatureSize]byte,
	publicKeys [][MLDSAPublicKeySize]byte,
	txHashes [][32]byte,
	blockHeight uint64,
) (*BlockProof, error) {
	if len(signatures) != 4096 {
		return nil, ErrInvalidInput
	}

	// Create C block proof struct
	var cProof C.aegis_maat_block_proof_t

	result := C.aegis_maat_generate_block_proof(
		&cProof,
		(*[MLDSASignatureSize]C.uint8_t)(unsafe.Pointer(&signatures[0])),
		(*[MLDSAPublicKeySize]C.uint8_t)(unsafe.Pointer(&publicKeys[0])),
		(*[32]C.uint8_t)(unsafe.Pointer(&txHashes[0])),
		C.uint64_t(blockHeight),
	)

	if result != 0 {
		return nil, ErrProverFailed
	}

	proof := &BlockProof{
		ProofLen:    uint64(cProof.proof_len),
		BlockHeight: uint64(cProof.block_height),
	}

	// Copy batch commits
	for i := 0; i < 16; i++ {
		for j := 0; j < 32; j++ {
			proof.BatchCommits[i][j] = byte(cProof.batch_commits[i][j])
		}
	}

	// Copy aggregated proof
	proof.AggregatedProof = make([]byte, cProof.proof_len)
	for i := uint64(0); i < uint64(cProof.proof_len); i++ {
		proof.AggregatedProof[i] = byte(cProof.aggregated_proof[i])
	}

	return proof, nil
}

// VerifyBlockProof verifies a MAAT block proof.
func VerifyBlockProof(
	proof *BlockProof,
	publicKeys [][MLDSAPublicKeySize]byte,
	txHashes [][32]byte,
) (bool, error) {
	if proof == nil {
		return false, ErrInvalidInput
	}

	var cProof C.aegis_maat_block_proof_t

	// Copy data to C struct
	for i := 0; i < 16; i++ {
		for j := 0; j < 32; j++ {
			cProof.batch_commits[i][j] = C.uint8_t(proof.BatchCommits[i][j])
		}
	}
	for i, b := range proof.AggregatedProof {
		cProof.aggregated_proof[i] = C.uint8_t(b)
	}
	cProof.proof_len = C.size_t(proof.ProofLen)
	cProof.block_height = C.uint64_t(proof.BlockHeight)

	var valid C.bool

	result := C.aegis_maat_verify_block_proof(
		&cProof,
		(*[MLDSAPublicKeySize]C.uint8_t)(unsafe.Pointer(&publicKeys[0])),
		(*[32]C.uint8_t)(unsafe.Pointer(&txHashes[0])),
		&valid,
	)

	if result != 0 {
		return false, ErrVerificationFailed
	}

	return bool(valid), nil
}

// VerifyEpochProof verifies a MAAT epoch proof for light client sync.
func VerifyEpochProof(proof *EpochProof, prevState, expectedState [32]byte) (bool, error) {
	if proof == nil {
		return false, ErrInvalidInput
	}

	var valid C.bool

	result := C.aegis_maat_verify_epoch_proof(
		(*C.aegis_maat_epoch_proof_t)(unsafe.Pointer(proof)),
		(*C.uint8_t)(unsafe.Pointer(&prevState[0])),
		(*C.uint8_t)(unsafe.Pointer(&expectedState[0])),
		&valid,
	)

	if result != 0 {
		return false, ErrVerificationFailed
	}

	return bool(valid), nil
}

// ============================================================================
// TEFNUT - Light Client Proofs
// ============================================================================

// LightClientProfile represents resource constraints
type LightClientProfile int

const (
	ProfileStandard LightClientProfile = 0 // 64KB RAM, 100ms verify
	ProfileUltra    LightClientProfile = 1 // 8KB RAM, 50ms verify
	ProfileIoT      LightClientProfile = 2 // ESP32/nRF52 optimized
)

// Checkpoint represents a light client checkpoint
type Checkpoint struct {
	BlockHeight   uint64
	StateRoot     [32]byte
	CommitteeRoot [32]byte
	Proof         []byte
}

// GenerateLightClientProof generates a proof for a state query.
func GenerateLightClientProof(
	profile LightClientProfile,
	checkpoint *Checkpoint,
	queryKey [32]byte,
	queryValue []byte,
) ([]byte, error) {
	if checkpoint == nil {
		return nil, ErrInvalidInput
	}

	type cCheckpoint struct {
		blockHeight   C.uint64_t
		stateRoot     [32]C.uint8_t
		committeeRoot [32]C.uint8_t
		proofData     [1024]C.uint8_t
		proofLen      C.size_t
	}
	var cCP cCheckpoint
	cCP.blockHeight = C.uint64_t(checkpoint.BlockHeight)
	for i := 0; i < 32; i++ {
		cCP.stateRoot[i] = C.uint8_t(checkpoint.StateRoot[i])
		cCP.committeeRoot[i] = C.uint8_t(checkpoint.CommitteeRoot[i])
	}
	for i, b := range checkpoint.Proof {
		if i >= 1024 {
			break
		}
		cCP.proofData[i] = C.uint8_t(b)
	}
	cCP.proofLen = C.size_t(len(checkpoint.Proof))

	proof := make([]byte, 4096)
	var proofLen C.size_t

	var valuePtr *C.uint8_t
	if len(queryValue) > 0 {
		valuePtr = (*C.uint8_t)(unsafe.Pointer(&queryValue[0]))
	}

	result := C.aegis_tefnut_generate_proof(
		(*C.uint8_t)(unsafe.Pointer(&proof[0])),
		&proofLen,
		C.aegis_tefnut_profile_t(profile),
		(*C.aegis_tefnut_checkpoint_t)(unsafe.Pointer(&cCP)),
		(*C.uint8_t)(unsafe.Pointer(&queryKey[0])),
		valuePtr,
		C.size_t(len(queryValue)),
	)

	if result != 0 {
		return nil, ErrProverFailed
	}

	return proof[:proofLen], nil
}

// VerifyLightClientProof verifies a light client proof.
func VerifyLightClientProof(
	checkpoint *Checkpoint,
	queryKey [32]byte,
	queryValue []byte,
	proof []byte,
) (bool, error) {
	if checkpoint == nil || len(proof) == 0 {
		return false, ErrInvalidInput
	}

	type cCheckpoint struct {
		blockHeight   C.uint64_t
		stateRoot     [32]C.uint8_t
		committeeRoot [32]C.uint8_t
		proofData     [1024]C.uint8_t
		proofLen      C.size_t
	}
	var cCP cCheckpoint
	cCP.blockHeight = C.uint64_t(checkpoint.BlockHeight)
	for i := 0; i < 32; i++ {
		cCP.stateRoot[i] = C.uint8_t(checkpoint.StateRoot[i])
		cCP.committeeRoot[i] = C.uint8_t(checkpoint.CommitteeRoot[i])
	}

	var valid C.bool
	var valuePtr *C.uint8_t
	if len(queryValue) > 0 {
		valuePtr = (*C.uint8_t)(unsafe.Pointer(&queryValue[0]))
	}

	result := C.aegis_tefnut_verify_proof(
		(*C.aegis_tefnut_checkpoint_t)(unsafe.Pointer(&cCP)),
		(*C.uint8_t)(unsafe.Pointer(&queryKey[0])),
		valuePtr,
		C.size_t(len(queryValue)),
		(*C.uint8_t)(unsafe.Pointer(&proof[0])),
		C.size_t(len(proof)),
		&valid,
	)

	if result != 0 {
		return false, ErrVerificationFailed
	}

	return bool(valid), nil
}

// ============================================================================
// SEKHMET - SIMD Acceleration
// ============================================================================

// DetectSIMDBackend returns the best available SIMD backend.
func DetectSIMDBackend() int {
	return int(C.aegis_sekhmet_detect_backend())
}

// NTT performs a vectorized Number Theoretic Transform.
func NTT(data []uint64, inverse bool) error {
	if len(data) == 0 {
		return ErrInvalidInput
	}

	result := C.aegis_sekhmet_ntt(
		(*C.uint64_t)(unsafe.Pointer(&data[0])),
		C.size_t(len(data)),
		C.bool(inverse),
	)

	if result != 0 {
		return errors.New("NTT failed")
	}

	return nil
}

// PolyMul performs vectorized polynomial multiplication.
func PolyMul(a, b []uint64) ([]uint64, error) {
	if len(a) != len(b) || len(a) == 0 {
		return nil, ErrInvalidInput
	}

	result := make([]uint64, len(a))

	res := C.aegis_sekhmet_poly_mul(
		(*C.uint64_t)(unsafe.Pointer(&result[0])),
		(*C.uint64_t)(unsafe.Pointer(&a[0])),
		(*C.uint64_t)(unsafe.Pointer(&b[0])),
		C.size_t(len(a)),
	)

	if res != 0 {
		return nil, errors.New("polynomial multiplication failed")
	}

	return result, nil
}

// ============================================================================
// KHEPRI - Native SPARK Contract System
// ============================================================================

// DeployResult represents the result of deploying a SPARK contract
type DeployResult struct {
	ContractAddr [32]byte
	CodeHash     [32]byte
	CertLevel    int
	GasUsed      uint64
}

// DeployContract deploys a native SPARK contract.
func DeployContract(
	elfBinary []byte,
	proofHash [32]byte,
	certLevel int,
	deployer [32]byte,
	gasLimit uint64,
) (*DeployResult, error) {
	if len(elfBinary) == 0 {
		return nil, ErrInvalidInput
	}

	var cResult C.aegis_khepri_deploy_result_t

	result := C.aegis_khepri_deploy(
		&cResult,
		(*C.uint8_t)(unsafe.Pointer(&elfBinary[0])),
		C.size_t(len(elfBinary)),
		(*C.uint8_t)(unsafe.Pointer(&proofHash[0])),
		C.aegis_cert_level_t(certLevel),
		(*C.uint8_t)(unsafe.Pointer(&deployer[0])),
		C.uint64_t(gasLimit),
	)

	if result != 0 {
		return nil, errors.New("contract deployment failed")
	}

	deployResult := &DeployResult{
		CertLevel: int(cResult.cert_level),
		GasUsed:   uint64(cResult.gas_used),
	}
	for i := 0; i < 32; i++ {
		deployResult.ContractAddr[i] = byte(cResult.contract_addr[i])
		deployResult.CodeHash[i] = byte(cResult.code_hash[i])
	}

	return deployResult, nil
}

// VerifyCertification verifies a contract's certification level.
func VerifyCertification(codeHash, proofHash [32]byte, claimedLevel int) (bool, error) {
	var valid C.bool

	result := C.aegis_khepri_verify_certification(
		(*C.uint8_t)(unsafe.Pointer(&codeHash[0])),
		(*C.uint8_t)(unsafe.Pointer(&proofHash[0])),
		C.aegis_cert_level_t(claimedLevel),
		&valid,
	)

	if result != 0 {
		return false, ErrVerificationFailed
	}

	return bool(valid), nil
}
