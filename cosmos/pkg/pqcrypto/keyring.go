// Package pqcrypto provides keyring integration for ML-DSA-87 keys.
package pqcrypto

import (
	"github.com/cosmos/cosmos-sdk/crypto/hd"
	"github.com/cosmos/cosmos-sdk/crypto/keyring"
	cryptotypes "github.com/cosmos/cosmos-sdk/crypto/types"
)

const (
	// MLDSAAlgorithm is the algorithm name for the keyring
	MLDSAAlgorithm = "ml-dsa"
)

var (
	// MLDSASupportedAlgos defines the list of signing algorithms
	MLDSASupportedAlgos = keyring.SigningAlgoList{MLDSAAlgo{}}

	// MLDSASupportedAlgosLedger defines algorithms for Ledger (not supported yet)
	MLDSASupportedAlgosLedger = keyring.SigningAlgoList{}
)

// MLDSAAlgo implements keyring.SignatureAlgo for ML-DSA-87.
type MLDSAAlgo struct{}

// Name returns the algorithm name.
func (a MLDSAAlgo) Name() hd.PubKeyType {
	return hd.PubKeyType(MLDSAAlgorithm)
}

// Derive derives a key from seed and path.
// Note: ML-DSA doesn't use BIP32/44 derivation, so we use the seed directly.
func (a MLDSAAlgo) Derive() hd.DeriveFn {
	return func(mnemonic string, bip39Passphrase, hdPath string) ([]byte, error) {
		// For ML-DSA, we derive a 32-byte seed from the mnemonic
		// This is different from secp256k1's BIP32 derivation
		seed, err := bip39SeedFromMnemonic(mnemonic, bip39Passphrase)
		if err != nil {
			return nil, err
		}
		// Use SHA3-256 to derive a deterministic seed for ML-DSA
		// incorporating the HD path for uniqueness
		derivedSeed := deriveMLDSASeed(seed, hdPath)
		return derivedSeed, nil
	}
}

// Generate generates a key from the derived seed.
func (a MLDSAAlgo) Generate() hd.GenerateFn {
	return func(bz []byte) cryptotypes.PrivKey {
		// Use the derived bytes as a seed for ML-DSA keygen
		_, sk, err := GenerateKeyFromSeed(bz)
		if err != nil {
			panic(err)
		}
		// Return SDK-wrapped key for Cosmos SDK compatibility
		return &SDKPrivKey{&PrivKey{Key: sk}}
	}
}

// bip39SeedFromMnemonic derives a BIP39 seed from mnemonic.
func bip39SeedFromMnemonic(mnemonic, passphrase string) ([]byte, error) {
	// Use SHA3-256 for post-quantum resistance
	// Combine mnemonic with salt as per BIP39
	salt := "mnemonic" + passphrase
	combined := mnemonic + salt
	hash := Hash([]byte(combined))
	return hash, nil
}

// deriveMLDSASeed derives an ML-DSA seed from a master seed and HD path.
func deriveMLDSASeed(masterSeed []byte, hdPath string) []byte {
	// Combine master seed with path for deterministic derivation
	// This doesn't follow BIP32 (which is ECDSA-specific) but provides
	// deterministic key derivation for ML-DSA
	combined := append(masterSeed, []byte(hdPath)...)
	return Hash(combined)
}

// GenerateKeyFromSeed generates an ML-DSA keypair from a 32-byte seed.
func GenerateKeyFromSeed(seed []byte) (PublicKey, SecretKey, error) {
	// Expand seed to 32 bytes if needed
	var seedBuf [SeedSize]byte
	if len(seed) >= SeedSize {
		copy(seedBuf[:], seed[:SeedSize])
	} else {
		expanded := Hash(seed)
		copy(seedBuf[:], expanded)
	}

	// Generate keypair with deterministic seed
	return GenerateKeyDeterministic(seedBuf)
}
