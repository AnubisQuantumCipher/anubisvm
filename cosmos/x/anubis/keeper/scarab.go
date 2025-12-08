// SCARAB v2.0 integration for x/anubis module
//
// This file integrates the SCARAB proof aggregation system with the Cosmos SDK:
// - KHNUM: TX signature aggregation
// - SEBEK: Threshold signatures (t-of-n ML-DSA-87)
// - MAAT: Hierarchical proof aggregation
// - HORUS: Pipelined prover market

package keeper

import (
	"context"
	"encoding/json"

	sdk "github.com/cosmos/cosmos-sdk/types"

	"github.com/aegisvm/cosmos/pkg/pqcrypto"
	"github.com/aegisvm/cosmos/pkg/scarab"
	"github.com/aegisvm/cosmos/x/anubis/types"
)

// ============================================================================
// KHNUM - TX Signature Aggregation
// ============================================================================

// AggregateBlockSignatures aggregates all signatures in a block into a single proof.
// This enables massive block compression (4096 sigs -> 4KB proof).
func (k Keeper) AggregateBlockSignatures(
	ctx context.Context,
	signatures [][pqcrypto.SignatureSize]byte,
	publicKeys [][pqcrypto.PublicKeySize]byte,
	txHashes [][32]byte,
) ([]byte, error) {
	sdkCtx := sdk.UnwrapSDKContext(ctx)

	// Convert to scarab types
	sigs := make([][scarab.MLDSASignatureSize]byte, len(signatures))
	pks := make([][scarab.MLDSAPublicKeySize]byte, len(publicKeys))
	for i := range signatures {
		copy(sigs[i][:], signatures[i][:])
		copy(pks[i][:], publicKeys[i][:])
	}

	// Aggregate using KHNUM
	proof, err := scarab.AggregateSignatures(sigs, pks, txHashes)
	if err != nil {
		k.Logger(ctx).Error("KHNUM aggregation failed", "count", len(signatures), "error", err)
		return nil, types.ErrAggregationFailed
	}

	k.Logger(ctx).Info("KHNUM aggregation complete",
		"block", sdkCtx.BlockHeight(),
		"sigs", len(signatures),
		"proof_size", len(proof),
	)

	return proof, nil
}

// VerifyAggregatedProof verifies an aggregated signature proof.
func (k Keeper) VerifyAggregatedProof(
	ctx context.Context,
	proof []byte,
	publicKeys [][pqcrypto.PublicKeySize]byte,
	txHashes [][32]byte,
) (bool, error) {
	// Convert to scarab types
	pks := make([][scarab.MLDSAPublicKeySize]byte, len(publicKeys))
	for i := range publicKeys {
		copy(pks[i][:], publicKeys[i][:])
	}

	valid, err := scarab.VerifyAggregatedSignatures(proof, pks, txHashes)
	if err != nil {
		k.Logger(ctx).Error("KHNUM verification failed", "error", err)
		return false, types.ErrVerificationFailed
	}

	return valid, nil
}

// ============================================================================
// SEBEK - Threshold Signatures
// ============================================================================

// ThresholdKeyShare represents a validator's share of the threshold key
type ThresholdKeyShare struct {
	Index      uint32
	Share      []byte
	Commitment [32]byte
}

// SignWithThreshold generates a partial signature using a threshold key share.
func (k Keeper) SignWithThreshold(
	ctx context.Context,
	share *ThresholdKeyShare,
	message []byte,
) ([]byte, error) {
	scarabShare := &scarab.KeyShare{
		Index:      share.Index,
		Commitment: share.Commitment,
	}
	copy(scarabShare.Share[:], share.Share)

	partial, err := scarab.SignPartial(scarabShare, message)
	if err != nil {
		k.Logger(ctx).Error("SEBEK partial sign failed", "index", share.Index, "error", err)
		return nil, types.ErrThresholdSignFailed
	}

	return partial, nil
}

// CombineThresholdSignatures combines partial signatures into a threshold signature.
func (k Keeper) CombineThresholdSignatures(
	ctx context.Context,
	partials [][]byte,
	indices []uint32,
) ([]byte, error) {
	combined, err := scarab.CombinePartialSignatures(partials, indices)
	if err != nil {
		k.Logger(ctx).Error("SEBEK combine failed", "count", len(partials), "error", err)
		return nil, types.ErrThresholdCombineFailed
	}

	k.Logger(ctx).Info("SEBEK threshold signature combined",
		"signers", len(partials),
	)

	return combined, nil
}

// VerifyThresholdSignature verifies a threshold signature.
func (k Keeper) VerifyThresholdSignature(
	ctx context.Context,
	signature []byte,
	message []byte,
	combinedPK [32]byte,
) (bool, error) {
	valid, err := scarab.VerifyThresholdSignature(signature, message, combinedPK)
	if err != nil {
		k.Logger(ctx).Error("SEBEK verify failed", "error", err)
		return false, types.ErrThresholdVerifyFailed
	}

	return valid, nil
}

// ============================================================================
// MAAT - Hierarchical Proof Aggregation
// ============================================================================

// GenerateBlockProof generates a MAAT block proof for 4096 transactions.
func (k Keeper) GenerateBlockProof(
	ctx context.Context,
	signatures [][pqcrypto.SignatureSize]byte,
	publicKeys [][pqcrypto.PublicKeySize]byte,
	txHashes [][32]byte,
) (*scarab.BlockProof, error) {
	sdkCtx := sdk.UnwrapSDKContext(ctx)

	if len(signatures) != 4096 {
		return nil, types.ErrInvalidBatchSize
	}

	// Convert types
	sigs := make([][scarab.MLDSASignatureSize]byte, len(signatures))
	pks := make([][scarab.MLDSAPublicKeySize]byte, len(publicKeys))
	for i := range signatures {
		copy(sigs[i][:], signatures[i][:])
		copy(pks[i][:], publicKeys[i][:])
	}

	proof, err := scarab.GenerateBlockProof(sigs, pks, txHashes, uint64(sdkCtx.BlockHeight()))
	if err != nil {
		k.Logger(ctx).Error("MAAT block proof failed", "error", err)
		return nil, types.ErrProofGenerationFailed
	}

	k.Logger(ctx).Info("MAAT block proof generated",
		"block", sdkCtx.BlockHeight(),
		"proof_size", proof.ProofLen,
	)

	return proof, nil
}

// VerifyBlockProof verifies a MAAT block proof.
func (k Keeper) VerifyBlockProof(
	ctx context.Context,
	proof *scarab.BlockProof,
	publicKeys [][pqcrypto.PublicKeySize]byte,
	txHashes [][32]byte,
) (bool, error) {
	// Convert types
	pks := make([][scarab.MLDSAPublicKeySize]byte, len(publicKeys))
	for i := range publicKeys {
		copy(pks[i][:], publicKeys[i][:])
	}

	valid, err := scarab.VerifyBlockProof(proof, pks, txHashes)
	if err != nil {
		k.Logger(ctx).Error("MAAT block verify failed", "error", err)
		return false, types.ErrProofVerificationFailed
	}

	return valid, nil
}

// VerifyEpochProof verifies a MAAT epoch proof for light client sync.
func (k Keeper) VerifyEpochProof(
	ctx context.Context,
	proof *scarab.EpochProof,
	prevState [32]byte,
	expectedState [32]byte,
) (bool, error) {
	valid, err := scarab.VerifyEpochProof(proof, prevState, expectedState)
	if err != nil {
		k.Logger(ctx).Error("MAAT epoch verify failed", "error", err)
		return false, types.ErrProofVerificationFailed
	}

	return valid, nil
}

// ============================================================================
// TEFNUT - Light Client Proofs
// ============================================================================

// GenerateLightClientProof generates a proof for a state query.
func (k Keeper) GenerateLightClientProof(
	ctx context.Context,
	profile scarab.LightClientProfile,
	checkpoint *scarab.Checkpoint,
	queryKey [32]byte,
	queryValue []byte,
) ([]byte, error) {
	proof, err := scarab.GenerateLightClientProof(profile, checkpoint, queryKey, queryValue)
	if err != nil {
		k.Logger(ctx).Error("TEFNUT proof generation failed", "error", err)
		return nil, types.ErrLightClientProofFailed
	}

	return proof, nil
}

// VerifyLightClientProof verifies a light client proof.
func (k Keeper) VerifyLightClientProof(
	ctx context.Context,
	checkpoint *scarab.Checkpoint,
	queryKey [32]byte,
	queryValue []byte,
	proof []byte,
) (bool, error) {
	valid, err := scarab.VerifyLightClientProof(checkpoint, queryKey, queryValue, proof)
	if err != nil {
		k.Logger(ctx).Error("TEFNUT proof verification failed", "error", err)
		return false, types.ErrLightClientVerifyFailed
	}

	return valid, nil
}

// ============================================================================
// HORUS - Prover Market (Uses existing prover infrastructure from provers.go)
// ============================================================================

// GetPendingProofJobs returns jobs available for claiming
func (k Keeper) GetPendingProofJobs(ctx context.Context) []types.ProofJobInfo {
	sdkCtx := sdk.UnwrapSDKContext(ctx)
	store := sdkCtx.KVStore(k.storeKey)
	iterator := store.Iterator(types.ProofJobKeyPrefix, nil)
	defer iterator.Close()

	var jobs []types.ProofJobInfo
	for ; iterator.Valid(); iterator.Next() {
		var job types.ProofJobInfo
		if err := json.Unmarshal(iterator.Value(), &job); err != nil {
			continue
		}
		if job.Status == types.ProofJobStatusPending {
			jobs = append(jobs, job)
		}
	}

	return jobs
}

// PostProofJob posts a new proof job to the market
func (k Keeper) PostProofJob(
	ctx context.Context,
	batchData []byte,
	batchSize uint64,
	deadline uint64,
	reward uint64,
) ([]byte, error) {
	sdkCtx := sdk.UnwrapSDKContext(ctx)

	// Generate job ID
	input := append(batchData[:32], uint64ToBytes(uint64(sdkCtx.BlockHeight()))...)
	jobID, err := pqcrypto.SHA3_256(input)
	if err != nil {
		return nil, err
	}

	job := types.ProofJobInfo{
		JobID:       jobID[:],
		BatchHash:   batchData[:32],
		BatchSize:   batchSize,
		BlockHeight: uint64(sdkCtx.BlockHeight()),
		Deadline:    deadline,
		Reward:      reward,
		Status:      types.ProofJobStatusPending,
	}

	// Store job using JSON encoding
	store := sdkCtx.KVStore(k.storeKey)
	key := types.GetProofJobKey(jobID[:])
	bz, _ := json.Marshal(&job)
	store.Set(key, bz)

	k.Logger(ctx).Info("proof job posted",
		"job_id", jobID[:8],
		"batch_size", batchSize,
		"reward", reward,
	)

	return jobID[:], nil
}

// ClaimProofJob claims a proof job for a prover
func (k Keeper) ClaimProofJob(
	ctx context.Context,
	proverAddr sdk.AccAddress,
	jobID []byte,
) error {
	sdkCtx := sdk.UnwrapSDKContext(ctx)
	store := sdkCtx.KVStore(k.storeKey)

	// Get job
	key := types.GetProofJobKey(jobID)
	bz := store.Get(key)
	if bz == nil {
		return types.ErrProofJobNotFound
	}

	var job types.ProofJobInfo
	if err := json.Unmarshal(bz, &job); err != nil {
		return err
	}

	if job.Status != types.ProofJobStatusPending {
		return types.ErrProofJobNotAvailable
	}

	// Verify prover is registered using existing GetProver
	prover, found := k.GetProver(ctx, proverAddr)
	if !found || prover.Status != types.ProverStatusActive {
		return types.ErrProverNotRegistered
	}

	// Claim job
	job.Status = types.ProofJobStatusClaimed
	job.ClaimedBy = proverAddr.Bytes()
	job.ClaimedAt = uint64(sdkCtx.BlockHeight())

	bz, _ = json.Marshal(&job)
	store.Set(key, bz)

	k.Logger(ctx).Info("proof job claimed",
		"job_id", jobID[:8],
		"prover", proverAddr.String(),
	)

	return nil
}

// SubmitProofForJob submits a completed proof for a job
func (k Keeper) SubmitProofForJob(
	ctx context.Context,
	proverAddr sdk.AccAddress,
	jobID []byte,
	proof []byte,
) (uint64, error) {
	sdkCtx := sdk.UnwrapSDKContext(ctx)
	store := sdkCtx.KVStore(k.storeKey)

	// Get job
	key := types.GetProofJobKey(jobID)
	bz := store.Get(key)
	if bz == nil {
		return 0, types.ErrProofJobNotFound
	}

	var job types.ProofJobInfo
	if err := json.Unmarshal(bz, &job); err != nil {
		return 0, err
	}

	// Verify claimed by this prover
	if !bytesEqual(job.ClaimedBy, proverAddr.Bytes()) {
		return 0, types.ErrNotJobOwner
	}

	// Check deadline
	if uint64(sdkCtx.BlockHeight()) > job.Deadline {
		job.Status = types.ProofJobStatusExpired
		bz, _ = json.Marshal(&job)
		store.Set(key, bz)
		return 0, types.ErrProofDeadlinePassed
	}

	// Verify proof (basic validation - full verification happens in consensus)
	if len(proof) < 32 {
		return 0, types.ErrInvalidProof
	}

	// Mark job as submitted
	job.Status = types.ProofJobStatusSubmitted
	job.Proof = proof
	job.SubmittedAt = uint64(sdkCtx.BlockHeight())

	bz, _ = json.Marshal(&job)
	store.Set(key, bz)

	// Update prover stats using existing method
	k.IncrementProverStats(ctx, proverAddr)

	k.Logger(ctx).Info("proof submitted",
		"job_id", jobID[:8],
		"prover", proverAddr.String(),
		"proof_size", len(proof),
		"reward", job.Reward,
	)

	return job.Reward, nil
}

// ============================================================================
// SEKHMET - SIMD Acceleration
// ============================================================================

// GetSIMDBackend returns the detected SIMD backend
func (k Keeper) GetSIMDBackend() int {
	return scarab.DetectSIMDBackend()
}

// ============================================================================
// AADKG - Asynchronous Adaptive Distributed Key Generation
// ============================================================================

// GetAADKGState returns the current AADKG state for an epoch
func (k Keeper) GetAADKGState(ctx context.Context, epoch uint64) (*types.AADKGState, bool) {
	sdkCtx := sdk.UnwrapSDKContext(ctx)
	store := sdkCtx.KVStore(k.storeKey)
	key := types.GetAADKGStateKey(epoch)

	bz := store.Get(key)
	if bz == nil {
		return nil, false
	}

	// Decode state (simple JSON for now)
	var state types.AADKGState
	if err := json.Unmarshal(bz, &state); err != nil {
		return nil, false
	}

	return &state, true
}

// SetAADKGState stores the AADKG state
func (k Keeper) SetAADKGState(ctx context.Context, state *types.AADKGState) {
	sdkCtx := sdk.UnwrapSDKContext(ctx)
	store := sdkCtx.KVStore(k.storeKey)
	key := types.GetAADKGStateKey(state.Epoch)

	bz, _ := json.Marshal(state)
	store.Set(key, bz)
}

// InitializeAADKG starts a new DKG round
func (k Keeper) InitializeAADKG(
	ctx context.Context,
	epoch uint64,
	participants uint32,
	threshold uint32,
) error {
	sdkCtx := sdk.UnwrapSDKContext(ctx)

	if _, found := k.GetAADKGState(ctx, epoch); found {
		return types.ErrDKGWrongPhase
	}

	state := &types.AADKGState{
		Epoch:        epoch,
		Phase:        types.AADKGPhaseCommitment,
		Participants: participants,
		Threshold:    threshold,
		StartBlock:   uint64(sdkCtx.BlockHeight()),
	}

	k.SetAADKGState(ctx, state)

	k.Logger(ctx).Info("AADKG initialized",
		"epoch", epoch,
		"participants", participants,
		"threshold", threshold,
	)

	return nil
}

// AdvanceAADKGPhase advances the DKG state machine
func (k Keeper) AdvanceAADKGPhase(ctx context.Context, epoch uint64) error {
	state, found := k.GetAADKGState(ctx, epoch)
	if !found {
		return types.ErrDKGNotInitialized
	}

	switch state.Phase {
	case types.AADKGPhaseCommitment:
		state.Phase = types.AADKGPhaseDistribution
	case types.AADKGPhaseDistribution:
		state.Phase = types.AADKGPhaseVerification
	case types.AADKGPhaseVerification:
		state.Phase = types.AADKGPhaseAggregation
	case types.AADKGPhaseAggregation:
		state.Phase = types.AADKGPhaseComplete
	default:
		return types.ErrDKGWrongPhase
	}

	k.SetAADKGState(ctx, state)

	k.Logger(ctx).Info("AADKG phase advanced",
		"epoch", epoch,
		"new_phase", state.Phase,
	)

	return nil
}

// StoreEncryptedShare stores an encrypted DKG share
func (k Keeper) StoreEncryptedShare(
	ctx context.Context,
	epoch uint64,
	share *types.EncryptedShare,
) error {
	state, found := k.GetAADKGState(ctx, epoch)
	if !found {
		return types.ErrDKGNotInitialized
	}

	if state.Phase != types.AADKGPhaseDistribution {
		return types.ErrDKGWrongPhase
	}

	// Store share in dedicated storage
	sdkCtx := sdk.UnwrapSDKContext(ctx)
	store := sdkCtx.KVStore(k.storeKey)

	// Key: epoch || sender || recipient
	key := make([]byte, 8+4+4)
	for i := 0; i < 8; i++ {
		key[i] = byte(epoch >> (56 - 8*i))
	}
	key[8] = byte(share.Sender >> 24)
	key[9] = byte(share.Sender >> 16)
	key[10] = byte(share.Sender >> 8)
	key[11] = byte(share.Sender)
	key[12] = byte(share.Recipient >> 24)
	key[13] = byte(share.Recipient >> 16)
	key[14] = byte(share.Recipient >> 8)
	key[15] = byte(share.Recipient)

	bz, _ := json.Marshal(share)
	store.Set(append([]byte("aadkg_share_"), key...), bz)

	return nil
}

// CompleteAADKG finalizes the DKG and stores the combined public key
func (k Keeper) CompleteAADKG(
	ctx context.Context,
	epoch uint64,
	combinedPK []byte,
	committeeRoot []byte,
) error {
	state, found := k.GetAADKGState(ctx, epoch)
	if !found {
		return types.ErrDKGNotInitialized
	}

	if state.Phase != types.AADKGPhaseAggregation {
		return types.ErrDKGWrongPhase
	}

	state.Phase = types.AADKGPhaseComplete
	state.CombinedPK = combinedPK
	state.CommitteeRoot = committeeRoot

	k.SetAADKGState(ctx, state)

	k.Logger(ctx).Info("AADKG completed",
		"epoch", epoch,
		"combined_pk_len", len(combinedPK),
	)

	return nil
}

// Helper functions (uint64ToBytes, bytesEqual) are defined in keeper.go
