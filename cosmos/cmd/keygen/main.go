// Keygen tool for generating ML-DSA-87 keypairs with AAS-001 addresses
//
// This tool generates a new ML-DSA-87 keypair for the ANUBIS network
// and outputs the canonical AAS-001 v3.1 address.

package main

import (
	"crypto/rand"
	"encoding/hex"
	"flag"
	"fmt"
	"os"
	"strings"

	"github.com/aegisvm/cosmos/pkg/pqcrypto"
	"golang.org/x/crypto/sha3"
)

func main() {
	network := flag.String("network", "main", "Network type: main, test, dev, lab, staging")
	addrType := flag.String("type", "u", "Address type: u=user, v=validator, c=contract, s=system")
	outputSK := flag.Bool("output-sk", false, "Output secret key (DANGER: handle securely)")
	flag.Parse()

	// Generate ML-DSA-87 keypair
	pk, sk, err := pqcrypto.GenerateKey(rand.Reader)
	if err != nil {
		fmt.Fprintf(os.Stderr, "ERROR: Key generation failed: %v\n", err)
		os.Exit(1)
	}

	// Derive account ID using domain-separated SHA3-256 (pure Go)
	// Domain separator format: "aegis-v1-mldsa87-<type>"
	domainSep := "aegis-v1-mldsa87-" + *addrType
	hasher := sha3.New256()
	hasher.Write([]byte(domainSep))
	hasher.Write(pk[:])
	accountID := hasher.Sum(nil)

	// Format AAS-001 address (pure Go implementation)
	addr := formatAAS001Address(*network, *addrType, accountID)

	// Output
	fmt.Println("================================================================================")
	fmt.Println("                    ANUBIS ML-DSA-87 KEYPAIR GENERATED")
	fmt.Println("================================================================================")
	fmt.Println()
	fmt.Println("Algorithm:      ML-DSA-87 (NIST FIPS 204, Level 5)")
	fmt.Println("Network:       ", *network)
	fmt.Println("Address Type:  ", *addrType)
	fmt.Println()
	fmt.Println("--------------------------------------------------------------------------------")
	fmt.Println("AAS-001 ADDRESS (Canonical Format)")
	fmt.Println("--------------------------------------------------------------------------------")
	fmt.Println()
	fmt.Println(addr)
	fmt.Println()
	fmt.Println("--------------------------------------------------------------------------------")
	fmt.Println("ACCOUNT ID (32 bytes, hex)")
	fmt.Println("--------------------------------------------------------------------------------")
	fmt.Println()
	fmt.Println(hex.EncodeToString(accountID))
	fmt.Println()
	fmt.Println("--------------------------------------------------------------------------------")
	fmt.Println("PUBLIC KEY (2592 bytes, hex)")
	fmt.Println("--------------------------------------------------------------------------------")
	fmt.Println()
	fmt.Println(hex.EncodeToString(pk[:]))
	fmt.Println()

	if *outputSK {
		fmt.Println("--------------------------------------------------------------------------------")
		fmt.Println("SECRET KEY (4896 bytes, hex) *** KEEP THIS SECURE ***")
		fmt.Println("--------------------------------------------------------------------------------")
		fmt.Println()
		fmt.Println(hex.EncodeToString(sk[:]))
		fmt.Println()
	} else {
		fmt.Println("NOTE: Secret key not shown. Use --output-sk to display (handle securely).")
		fmt.Println()
	}

	fmt.Println("================================================================================")
	fmt.Println("BUILDER ALLOCATION: 300,000,000 ANUBIS (30%)")
	fmt.Println("================================================================================")
	fmt.Println()
	fmt.Println("This address is designated for the solo builder allocation per ANUBIS Token")
	fmt.Println("Specification v4.0. The 300M ANUBIS will vest based on 9 milestones over 6 years:")
	fmt.Println()
	fmt.Println("  Milestone 1: ML-DSA-87 SPARK Implementation  - 50,000,000 ANUBIS")
	fmt.Println("  Milestone 2: ML-KEM-1024 SPARK Implementation - 50,000,000 ANUBIS")
	fmt.Println("  Milestone 3: Core VM with WCET Gas Model     - 50,000,000 ANUBIS")
	fmt.Println("  Milestone 4: Cosmos SDK PQ Integration       - 50,000,000 ANUBIS")
	fmt.Println("  Milestone 5: zk-STARK Prover (Basic)         - 30,000,000 ANUBIS")
	fmt.Println("  Milestone 6: Privacy Layer (Shield, Whisper) - 30,000,000 ANUBIS")
	fmt.Println("  Milestone 7: Mainnet Launch                  - 20,000,000 ANUBIS")
	fmt.Println("  Milestone 8: 1 Year Mainnet Stability        - 10,000,000 ANUBIS")
	fmt.Println("  Milestone 9: 2 Year Mainnet Stability        - 10,000,000 ANUBIS")
	fmt.Println()
	fmt.Println("================================================================================")
}

// Crockford Base32 alphabet (excludes I, L, O, U to avoid confusion)
const crockfordBase32 = "0123456789abcdefghjkmnpqrstvwxyz"

// formatAAS001Address creates a canonical AAS-001 v3.1 address string
// Format: mldsa87:network:type:chunked_payload-checksum
func formatAAS001Address(network, addrType string, accountID []byte) string {
	// Encode account ID to Crockford Base32
	payload := encodeCrockfordBase32(accountID)

	// Chunk payload with dashes (8-char chunks, last chunk variable)
	chunked := chunkString(payload, 8)

	// Compute checksum over full identity (algorithm + network + type + account_id)
	identity := "mldsa87:" + network + ":" + addrType + ":" + hex.EncodeToString(accountID)
	checksumHash := sha3.Sum256([]byte(identity))
	checksum := encodeCrockfordBase32(checksumHash[:3])[:5] // 5-char checksum

	// Build final address
	return "mldsa87:" + network + ":" + addrType + ":" + chunked + "-" + checksum
}

// encodeCrockfordBase32 encodes bytes to Crockford Base32
func encodeCrockfordBase32(data []byte) string {
	if len(data) == 0 {
		return ""
	}

	var result strings.Builder
	buffer := uint64(0)
	bitsInBuffer := 0

	for _, b := range data {
		buffer = (buffer << 8) | uint64(b)
		bitsInBuffer += 8

		for bitsInBuffer >= 5 {
			bitsInBuffer -= 5
			idx := (buffer >> uint(bitsInBuffer)) & 0x1F
			result.WriteByte(crockfordBase32[idx])
		}
	}

	// Handle remaining bits
	if bitsInBuffer > 0 {
		idx := (buffer << uint(5-bitsInBuffer)) & 0x1F
		result.WriteByte(crockfordBase32[idx])
	}

	return result.String()
}

// chunkString splits a string into chunks of specified size with dashes
func chunkString(s string, size int) string {
	var chunks []string
	for i := 0; i < len(s); i += size {
		end := i + size
		if end > len(s) {
			end = len(s)
		}
		chunks = append(chunks, s[i:end])
	}
	return strings.Join(chunks, "-")
}
