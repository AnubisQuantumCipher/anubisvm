// Custom address commands for AAS-001 format display

package main

import (
	"fmt"

	"github.com/spf13/cobra"

	"github.com/cosmos/cosmos-sdk/client"
	"github.com/cosmos/cosmos-sdk/crypto/keyring"

	"github.com/aegisvm/cosmos/pkg/pqcrypto"
)

// AddressCmd returns command to show addresses in AAS-001 format
func AddressCmd() *cobra.Command {
	cmd := &cobra.Command{
		Use:   "address [key-name]",
		Short: "Show address in AAS-001 canonical format",
		Long: `Display the address for a key in the canonical AAS-001 v3.1 format.

The AAS-001 format is self-describing and quantum-resistant:
  mldsa87:test:u:qr7zy5kx-mjgpv4wc-8h2d6nft-3se09ax7-k5n2p8rv-wd4h7mcq-j9f3-v8k4n

Components:
  - mldsa87: Algorithm (ML-DSA-87, NIST Level 5)
  - test:    Network (main/test/dev)
  - u:       Type (u=user, c=contract, v=validator, s=system)
  - payload: 52-char Crockford Base32 encoded account ID
  - checksum: 5-char checksum covering full identity
`,
		Args: cobra.ExactArgs(1),
		RunE: func(cmd *cobra.Command, args []string) error {
			clientCtx, err := client.GetClientQueryContext(cmd)
			if err != nil {
				return err
			}

			keyName := args[0]

			// Get the key from keyring
			kr, err := keyring.New("aegis", keyring.BackendTest, clientCtx.HomeDir, nil, clientCtx.Codec)
			if err != nil {
				return fmt.Errorf("failed to open keyring: %w", err)
			}

			keyInfo, err := kr.Key(keyName)
			if err != nil {
				return fmt.Errorf("key '%s' not found: %w", keyName, err)
			}

			pubKey, err := keyInfo.GetPubKey()
			if err != nil {
				return fmt.Errorf("failed to get public key: %w", err)
			}

			// Get the address bytes (32 bytes for ML-DSA-87)
			addrBytes := pubKey.Address().Bytes()

			// Determine network type from chain ID
			networkType := pqcrypto.NetworkTest // Default to testnet

			// Format using AAS-001 codec
			codec := pqcrypto.NewAASAddressCodec(networkType)
			aasAddr, err := codec.BytesToString(addrBytes)
			if err != nil {
				return fmt.Errorf("failed to format AAS-001 address: %w", err)
			}

			// Print results
			fmt.Printf("Key Name:     %s\n", keyName)
			fmt.Printf("Key Type:     %s\n", pubKey.Type())
			fmt.Printf("AAS-001:      %s\n", aasAddr)

			// Also show the raw account ID in hex for debugging
			fmt.Printf("Account ID:   %X\n", addrBytes)

			return nil
		},
	}

	return cmd
}
