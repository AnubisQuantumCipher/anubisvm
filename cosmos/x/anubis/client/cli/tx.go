// Transaction CLI commands for x/anubis

package cli

import (
	"encoding/hex"
	"fmt"
	"os"
	"strconv"

	"github.com/cosmos/cosmos-sdk/client"
	"github.com/cosmos/cosmos-sdk/client/flags"
	"github.com/cosmos/cosmos-sdk/client/tx"
	"github.com/spf13/cobra"

	"github.com/aegisvm/cosmos/pkg/pqcrypto"
	"github.com/aegisvm/cosmos/x/anubis/types"
)

// GetTxCmd returns the transaction commands for this module
func GetTxCmd() *cobra.Command {
	cmd := &cobra.Command{
		Use:                        types.ModuleName,
		Short:                      "Anubis privacy transaction subcommands",
		DisableFlagParsing:         true,
		SuggestionsMinimumDistance: 2,
		RunE:                       client.ValidateCmd,
	}

	cmd.AddCommand(
		CmdEnablePrivate(),
		CmdPrivateExecute(),
		CmdInitSession(),
		CmdCloseSession(),
		CmdSessionExecute(),
	)

	return cmd
}

// CmdEnablePrivate returns the enable private execution command
func CmdEnablePrivate() *cobra.Command {
	cmd := &cobra.Command{
		Use:   "enable [contract] [public-key-file]",
		Short: "Enable private execution for a contract",
		Long: `Enable private execution mode for a deployed contract.
This allows users to submit encrypted transactions that are
decrypted and executed confidentially.

The public key file should contain an ML-KEM-1024 public key.

Example:
  aegisd tx anubis enable aegis1abc... ./kem_public.key --from admin
`,
		Args: cobra.ExactArgs(2),
		RunE: func(cmd *cobra.Command, args []string) error {
			clientCtx, err := client.GetClientTxContext(cmd)
			if err != nil {
				return err
			}

			contractAddr := args[0]

			// Read public key file
			pubKeyBytes, err := os.ReadFile(args[1])
			if err != nil {
				return fmt.Errorf("failed to read public key file: %w", err)
			}
			if len(pubKeyBytes) != pqcrypto.KEMPublicKeySize {
				return fmt.Errorf("invalid public key size: expected %d, got %d",
					pqcrypto.KEMPublicKeySize, len(pubKeyBytes))
			}

			// Get flags
			encScheme, _ := cmd.Flags().GetString("encryption")
			maxCalls, _ := cmd.Flags().GetUint32("max-calls")
			authorizedStr, _ := cmd.Flags().GetStringArray("authorized")

			if encScheme == "" {
				encScheme = types.EncryptionSchemeAESGCM
			}

			msg := &types.MsgEnablePrivate{
				Sender:            clientCtx.GetFromAddress().String(),
				Contract:          contractAddr,
				PublicKey:         pubKeyBytes,
				EncryptionScheme:  encScheme,
				MaxPrivateCalls:   maxCalls,
				AuthorizedCallers: authorizedStr,
			}

			if err := msg.ValidateBasic(); err != nil {
				return err
			}

			return tx.GenerateOrBroadcastTxCLI(clientCtx, cmd.Flags(), msg)
		},
	}

	cmd.Flags().String("encryption", types.EncryptionSchemeAESGCM, "Encryption scheme (aes-256-gcm, fhe-cggi)")
	cmd.Flags().Uint32("max-calls", 100, "Maximum concurrent private calls")
	cmd.Flags().StringArray("authorized", []string{}, "Authorized caller addresses")

	flags.AddTxFlagsToCmd(cmd)

	return cmd
}

// CmdPrivateExecute returns the private execute command
func CmdPrivateExecute() *cobra.Command {
	cmd := &cobra.Command{
		Use:   "execute [contract] [encrypted-call-file]",
		Short: "Execute a contract function privately",
		Long: `Execute a contract function with encrypted inputs and outputs.
The encrypted call file should contain the pre-encrypted call data.

Use the 'aegis-keytool' utility to create encrypted calls:
  aegis-keytool encrypt-call --contract aegis1abc... --function 0x12345678 --args 0xdeadbeef

Example:
  aegisd tx anubis execute aegis1abc... ./encrypted_call.bin --from mykey
`,
		Args: cobra.ExactArgs(2),
		RunE: func(cmd *cobra.Command, args []string) error {
			clientCtx, err := client.GetClientTxContext(cmd)
			if err != nil {
				return err
			}

			contractAddr := args[0]

			// Read encrypted call file
			encCallBytes, err := os.ReadFile(args[1])
			if err != nil {
				return fmt.Errorf("failed to read encrypted call file: %w", err)
			}

			// Parse encrypted call structure
			// Format: [32 bytes call ID][encrypted function][encrypted args][1568 bytes KEM ciphertext][nonce][4627 bytes signature]
			if len(encCallBytes) < 32+4+0+pqcrypto.KEMCiphertextSize+16+pqcrypto.SignatureSize {
				return fmt.Errorf("encrypted call file too small")
			}

			expectedGas, _ := cmd.Flags().GetUint64("gas-estimate")

			encCall := types.EncryptedCall{
				ContractAddress: []byte(contractAddr), // Will be validated
				// Parse from encCallBytes...
			}

			msg := &types.MsgPrivateExecute{
				Sender:        clientCtx.GetFromAddress().String(),
				EncryptedCall: encCall,
				ExpectedGas:   expectedGas,
			}

			if err := msg.ValidateBasic(); err != nil {
				return err
			}

			return tx.GenerateOrBroadcastTxCLI(clientCtx, cmd.Flags(), msg)
		},
	}

	cmd.Flags().Uint64("gas-estimate", 100000, "Expected gas consumption")

	flags.AddTxFlagsToCmd(cmd)

	return cmd
}

// CmdInitSession returns the init session command
func CmdInitSession() *cobra.Command {
	cmd := &cobra.Command{
		Use:   "init-session [contract]",
		Short: "Initialize a private execution session",
		Long: `Initialize a new session for multiple private calls to a contract.
Sessions allow you to make multiple encrypted calls without re-establishing
the key encapsulation for each call.

Example:
  aegisd tx anubis init-session aegis1abc... --max-calls 10 --duration 100 --from mykey
`,
		Args: cobra.ExactArgs(1),
		RunE: func(cmd *cobra.Command, args []string) error {
			clientCtx, err := client.GetClientTxContext(cmd)
			if err != nil {
				return err
			}

			contractAddr := args[0]
			maxCalls, _ := cmd.Flags().GetUint64("max-calls")
			duration, _ := cmd.Flags().GetInt64("duration")

			// Generate KEM ciphertext for session key
			// In practice, this would be done with the contract's public key
			kemCiphertext := make([]byte, pqcrypto.KEMCiphertextSize)
			// This is a placeholder - real implementation would use pqcrypto.Encapsulate

			msg := &types.MsgInitSession{
				Sender:        clientCtx.GetFromAddress().String(),
				Contract:      contractAddr,
				KEMCiphertext: kemCiphertext,
				MaxCalls:      maxCalls,
				Duration:      duration,
			}

			if err := msg.ValidateBasic(); err != nil {
				return err
			}

			return tx.GenerateOrBroadcastTxCLI(clientCtx, cmd.Flags(), msg)
		},
	}

	cmd.Flags().Uint64("max-calls", 10, "Maximum calls in this session")
	cmd.Flags().Int64("duration", 100, "Session duration in blocks")

	flags.AddTxFlagsToCmd(cmd)

	return cmd
}

// CmdCloseSession returns the close session command
func CmdCloseSession() *cobra.Command {
	cmd := &cobra.Command{
		Use:   "close-session [session-id]",
		Short: "Close a private execution session",
		Long: `Close an active session and clean up associated state.

Example:
  aegisd tx anubis close-session 0x1234...abcd --from mykey
`,
		Args: cobra.ExactArgs(1),
		RunE: func(cmd *cobra.Command, args []string) error {
			clientCtx, err := client.GetClientTxContext(cmd)
			if err != nil {
				return err
			}

			// Parse session ID
			sessionIDStr := args[0]
			if len(sessionIDStr) >= 2 && sessionIDStr[:2] == "0x" {
				sessionIDStr = sessionIDStr[2:]
			}
			sessionID, err := hex.DecodeString(sessionIDStr)
			if err != nil {
				return fmt.Errorf("invalid session ID: %w", err)
			}

			msg := &types.MsgCloseSession{
				Sender:    clientCtx.GetFromAddress().String(),
				SessionID: sessionID,
			}

			if err := msg.ValidateBasic(); err != nil {
				return err
			}

			return tx.GenerateOrBroadcastTxCLI(clientCtx, cmd.Flags(), msg)
		},
	}

	flags.AddTxFlagsToCmd(cmd)

	return cmd
}

// CmdSessionExecute returns the session execute command
func CmdSessionExecute() *cobra.Command {
	cmd := &cobra.Command{
		Use:   "session-execute [session-id] [encrypted-function] [encrypted-args]",
		Short: "Execute within a session",
		Long: `Execute an encrypted function call within an active session.
The function and arguments should be encrypted with the session key.

Example:
  aegisd tx anubis session-execute 0x1234...abcd 0xabc... 0xdef... --nonce 1 --from mykey
`,
		Args: cobra.MinimumNArgs(2),
		RunE: func(cmd *cobra.Command, args []string) error {
			clientCtx, err := client.GetClientTxContext(cmd)
			if err != nil {
				return err
			}

			// Parse session ID
			sessionIDStr := args[0]
			if len(sessionIDStr) >= 2 && sessionIDStr[:2] == "0x" {
				sessionIDStr = sessionIDStr[2:]
			}
			sessionID, err := hex.DecodeString(sessionIDStr)
			if err != nil {
				return fmt.Errorf("invalid session ID: %w", err)
			}

			// Parse encrypted function
			encFuncStr := args[1]
			if len(encFuncStr) >= 2 && encFuncStr[:2] == "0x" {
				encFuncStr = encFuncStr[2:]
			}
			encFunction, err := hex.DecodeString(encFuncStr)
			if err != nil {
				return fmt.Errorf("invalid encrypted function: %w", err)
			}

			// Parse encrypted args
			var encArgs []byte
			if len(args) > 2 {
				encArgsStr := args[2]
				if len(encArgsStr) >= 2 && encArgsStr[:2] == "0x" {
					encArgsStr = encArgsStr[2:]
				}
				encArgs, err = hex.DecodeString(encArgsStr)
				if err != nil {
					return fmt.Errorf("invalid encrypted args: %w", err)
				}
			}

			nonceStr, _ := cmd.Flags().GetString("nonce")
			nonce, err := strconv.ParseUint(nonceStr, 10, 64)
			if err != nil {
				return fmt.Errorf("invalid nonce: %w", err)
			}

			msg := &types.MsgSessionExecute{
				Sender:            clientCtx.GetFromAddress().String(),
				SessionID:         sessionID,
				EncryptedFunction: encFunction,
				EncryptedArgs:     encArgs,
				Nonce:             nonce,
			}

			if err := msg.ValidateBasic(); err != nil {
				return err
			}

			return tx.GenerateOrBroadcastTxCLI(clientCtx, cmd.Flags(), msg)
		},
	}

	cmd.Flags().String("nonce", "0", "Call nonce within the session")

	flags.AddTxFlagsToCmd(cmd)

	return cmd
}
