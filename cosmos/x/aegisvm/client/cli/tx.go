// Transaction CLI commands for x/aegisvm

package cli

import (
	"encoding/hex"
	"fmt"
	"os"
	"strconv"

	"github.com/cosmos/cosmos-sdk/client"
	"github.com/cosmos/cosmos-sdk/client/flags"
	"github.com/cosmos/cosmos-sdk/client/tx"
	sdk "github.com/cosmos/cosmos-sdk/types"
	"github.com/spf13/cobra"

	"github.com/aegisvm/cosmos/x/aegisvm/types"
)

// GetTxCmd returns the transaction commands for this module
func GetTxCmd() *cobra.Command {
	cmd := &cobra.Command{
		Use:                        types.ModuleName,
		Short:                      "AegisVM transaction subcommands",
		DisableFlagParsing:         true,
		SuggestionsMinimumDistance: 2,
		RunE:                       client.ValidateCmd,
	}

	cmd.AddCommand(
		CmdDeployContract(),
		CmdExecuteContract(),
		CmdMigrateContract(),
		CmdUpdateAdmin(),
	)

	return cmd
}

// CmdDeployContract returns the deploy contract command
func CmdDeployContract() *cobra.Command {
	cmd := &cobra.Command{
		Use:   "deploy [bytecode-file] [proof-hash]",
		Short: "Deploy a new verified smart contract",
		Long: `Deploy a new smart contract with SPARK proof verification.

The bytecode file should contain the compiled AegisVM bytecode.
The proof hash is the SHA3-256 hash of the SPARK proof artifacts.

Example:
  aegisd tx aegisvm deploy ./contract.avm 0x1234...abcd --from mykey
`,
		Args: cobra.ExactArgs(2),
		RunE: func(cmd *cobra.Command, args []string) error {
			clientCtx, err := client.GetClientTxContext(cmd)
			if err != nil {
				return err
			}

			// Read bytecode file
			bytecode, err := os.ReadFile(args[0])
			if err != nil {
				return fmt.Errorf("failed to read bytecode file: %w", err)
			}

			// Parse proof hash
			proofHashStr := args[1]
			if len(proofHashStr) >= 2 && proofHashStr[:2] == "0x" {
				proofHashStr = proofHashStr[2:]
			}
			proofHash, err := hex.DecodeString(proofHashStr)
			if err != nil {
				return fmt.Errorf("invalid proof hash: %w", err)
			}
			if len(proofHash) != 32 {
				return fmt.Errorf("proof hash must be 32 bytes")
			}

			// Get optional flags
			initArgs, _ := cmd.Flags().GetString("init-args")
			katHash, _ := cmd.Flags().GetString("kat-hash")
			admin, _ := cmd.Flags().GetString("admin")
			label, _ := cmd.Flags().GetString("label")

			var initArgsBytes []byte
			if initArgs != "" {
				initArgsBytes, err = hex.DecodeString(initArgs)
				if err != nil {
					return fmt.Errorf("invalid init args: %w", err)
				}
			}

			var katHashBytes []byte
			if katHash != "" {
				katHashBytes, err = hex.DecodeString(katHash)
				if err != nil {
					return fmt.Errorf("invalid KAT hash: %w", err)
				}
			}

			msg := &types.MsgDeployContract{
				Sender:    clientCtx.GetFromAddress().String(),
				Bytecode:  bytecode,
				InitArgs:  initArgsBytes,
				Profile:   types.ProfilePQL5Strict,
				ProofHash: proofHash,
				KATHash:   katHashBytes,
				Admin:     admin,
				Label:     label,
			}

			if err := msg.ValidateBasic(); err != nil {
				return err
			}

			return tx.GenerateOrBroadcastTxCLI(clientCtx, cmd.Flags(), msg)
		},
	}

	cmd.Flags().String("init-args", "", "Hex-encoded initialization arguments")
	cmd.Flags().String("kat-hash", "", "SHA3-256 hash of KAT vectors")
	cmd.Flags().String("admin", "", "Admin address for contract upgrades")
	cmd.Flags().String("label", "", "Human-readable label for the contract")

	flags.AddTxFlagsToCmd(cmd)

	return cmd
}

// CmdExecuteContract returns the execute contract command
func CmdExecuteContract() *cobra.Command {
	cmd := &cobra.Command{
		Use:   "execute [contract] [function] [args]",
		Short: "Execute a smart contract function",
		Long: `Execute a function on a deployed smart contract.

The function should be specified as a 4-byte hex selector.
Arguments should be hex-encoded.

Example:
  aegisd tx aegisvm execute aegis1abc... 0x12345678 0xdeadbeef --from mykey
`,
		Args: cobra.MinimumNArgs(2),
		RunE: func(cmd *cobra.Command, args []string) error {
			clientCtx, err := client.GetClientTxContext(cmd)
			if err != nil {
				return err
			}

			contractAddr := args[0]

			// Parse function selector
			funcStr := args[1]
			if len(funcStr) >= 2 && funcStr[:2] == "0x" {
				funcStr = funcStr[2:]
			}
			function, err := hex.DecodeString(funcStr)
			if err != nil {
				return fmt.Errorf("invalid function selector: %w", err)
			}
			if len(function) != 4 {
				return fmt.Errorf("function selector must be 4 bytes")
			}

			// Parse arguments if provided
			var funcArgs []byte
			if len(args) > 2 {
				argsStr := args[2]
				if len(argsStr) >= 2 && argsStr[:2] == "0x" {
					argsStr = argsStr[2:]
				}
				funcArgs, err = hex.DecodeString(argsStr)
				if err != nil {
					return fmt.Errorf("invalid function arguments: %w", err)
				}
			}

			// Parse funds
			fundsStr, _ := cmd.Flags().GetString("amount")
			var funds sdk.Coins
			if fundsStr != "" {
				funds, err = sdk.ParseCoinsNormalized(fundsStr)
				if err != nil {
					return fmt.Errorf("invalid funds: %w", err)
				}
			}

			msg := &types.MsgExecuteContract{
				Sender:   clientCtx.GetFromAddress().String(),
				Contract: contractAddr,
				Function: function,
				Args:     funcArgs,
				Funds:    funds,
			}

			if err := msg.ValidateBasic(); err != nil {
				return err
			}

			return tx.GenerateOrBroadcastTxCLI(clientCtx, cmd.Flags(), msg)
		},
	}

	cmd.Flags().String("amount", "", "Coins to send with the execution")

	flags.AddTxFlagsToCmd(cmd)

	return cmd
}

// CmdMigrateContract returns the migrate contract command
func CmdMigrateContract() *cobra.Command {
	cmd := &cobra.Command{
		Use:   "migrate [contract] [new-bytecode-file] [new-proof-hash]",
		Short: "Migrate a contract to new bytecode",
		Long: `Migrate an existing contract to new bytecode.

Only the contract admin can perform this operation.
The new bytecode must also have SPARK proof verification.

Example:
  aegisd tx aegisvm migrate aegis1abc... ./new_contract.avm 0x5678...ef --from admin
`,
		Args: cobra.ExactArgs(3),
		RunE: func(cmd *cobra.Command, args []string) error {
			clientCtx, err := client.GetClientTxContext(cmd)
			if err != nil {
				return err
			}

			contractAddr := args[0]

			// Read new bytecode
			newBytecode, err := os.ReadFile(args[1])
			if err != nil {
				return fmt.Errorf("failed to read new bytecode file: %w", err)
			}

			// Parse new proof hash
			proofHashStr := args[2]
			if len(proofHashStr) >= 2 && proofHashStr[:2] == "0x" {
				proofHashStr = proofHashStr[2:]
			}
			newProofHash, err := hex.DecodeString(proofHashStr)
			if err != nil {
				return fmt.Errorf("invalid proof hash: %w", err)
			}

			// Parse migrate args
			migrateArgs, _ := cmd.Flags().GetString("migrate-args")
			var migrateArgsBytes []byte
			if migrateArgs != "" {
				migrateArgsBytes, err = hex.DecodeString(migrateArgs)
				if err != nil {
					return fmt.Errorf("invalid migrate args: %w", err)
				}
			}

			msg := &types.MsgMigrateContract{
				Sender:       clientCtx.GetFromAddress().String(),
				Contract:     contractAddr,
				NewBytecode:  newBytecode,
				NewProofHash: newProofHash,
				MigrateArgs:  migrateArgsBytes,
			}

			if err := msg.ValidateBasic(); err != nil {
				return err
			}

			return tx.GenerateOrBroadcastTxCLI(clientCtx, cmd.Flags(), msg)
		},
	}

	cmd.Flags().String("migrate-args", "", "Hex-encoded migration arguments")

	flags.AddTxFlagsToCmd(cmd)

	return cmd
}

// CmdUpdateAdmin returns the update admin command
func CmdUpdateAdmin() *cobra.Command {
	cmd := &cobra.Command{
		Use:   "update-admin [contract] [new-admin]",
		Short: "Update the admin of a contract",
		Long: `Update the admin address of a contract.

Only the current admin can perform this operation.
Set new-admin to empty string to remove admin (make contract immutable).

Example:
  aegisd tx aegisvm update-admin aegis1abc... aegis1xyz... --from current-admin
`,
		Args: cobra.ExactArgs(2),
		RunE: func(cmd *cobra.Command, args []string) error {
			clientCtx, err := client.GetClientTxContext(cmd)
			if err != nil {
				return err
			}

			msg := &types.MsgUpdateAdmin{
				Sender:   clientCtx.GetFromAddress().String(),
				Contract: args[0],
				NewAdmin: args[1],
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

// CmdApproveCode returns the approve code command (governance only)
func CmdApproveCode() *cobra.Command {
	cmd := &cobra.Command{
		Use:   "approve-code [code-hash] [proposal-id]",
		Short: "Mark a code hash as approved (requires governance)",
		Long: `Mark a bytecode hash as approved for deployment.

This is typically done through governance proposals.
The code hash must be approved before contracts using it can be deployed.

Example:
  aegisd tx aegisvm approve-code 0x1234...abcd 42 --from validator
`,
		Args: cobra.ExactArgs(2),
		RunE: func(cmd *cobra.Command, args []string) error {
			clientCtx, err := client.GetClientTxContext(cmd)
			if err != nil {
				return err
			}

			// Parse code hash
			codeHashStr := args[0]
			if len(codeHashStr) >= 2 && codeHashStr[:2] == "0x" {
				codeHashStr = codeHashStr[2:]
			}
			codeHash, err := hex.DecodeString(codeHashStr)
			if err != nil {
				return fmt.Errorf("invalid code hash: %w", err)
			}

			proposalID, err := strconv.ParseUint(args[1], 10, 64)
			if err != nil {
				return fmt.Errorf("invalid proposal ID: %w", err)
			}

			// This would be a governance proposal message
			_ = codeHash
			_ = proposalID
			_ = clientCtx

			return fmt.Errorf("code approval must be done through governance proposal")
		},
	}

	flags.AddTxFlagsToCmd(cmd)

	return cmd
}
