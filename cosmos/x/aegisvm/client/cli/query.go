// Query CLI commands for x/aegisvm

package cli

import (
	"context"
	"encoding/hex"
	"fmt"

	"github.com/cosmos/cosmos-sdk/client"
	"github.com/cosmos/cosmos-sdk/client/flags"
	"github.com/spf13/cobra"

	"github.com/aegisvm/cosmos/x/aegisvm/types"
)

// GetQueryCmd returns the query commands for this module
func GetQueryCmd() *cobra.Command {
	cmd := &cobra.Command{
		Use:                        types.ModuleName,
		Short:                      "Querying commands for the aegisvm module",
		DisableFlagParsing:         true,
		SuggestionsMinimumDistance: 2,
		RunE:                       client.ValidateCmd,
	}

	cmd.AddCommand(
		CmdQueryParams(),
		CmdQueryContract(),
		CmdQueryContractState(),
		CmdQueryBytecode(),
		CmdQueryContracts(),
		CmdQueryIsCodeApproved(),
		CmdSimulateExecution(),
	)

	return cmd
}

// CmdQueryParams returns the query params command
func CmdQueryParams() *cobra.Command {
	cmd := &cobra.Command{
		Use:   "params",
		Short: "Query the current module parameters",
		Long: `Query the current configuration parameters for the aegisvm module.

Example:
  aegisd query aegisvm params
`,
		Args: cobra.NoArgs,
		RunE: func(cmd *cobra.Command, args []string) error {
			clientCtx, err := client.GetClientQueryContext(cmd)
			if err != nil {
				return err
			}

			queryClient := types.NewQueryClient(clientCtx)

			res, err := queryClient.Params(context.Background(), &types.QueryParamsRequest{})
			if err != nil {
				return err
			}

			return clientCtx.PrintProto(res)
		},
	}

	flags.AddQueryFlagsToCmd(cmd)

	return cmd
}

// CmdQueryContract returns the query contract command
func CmdQueryContract() *cobra.Command {
	cmd := &cobra.Command{
		Use:   "contract [address]",
		Short: "Query a contract by address",
		Long: `Query detailed information about a deployed contract.

Example:
  aegisd query aegisvm contract aegis1abc...xyz
`,
		Args: cobra.ExactArgs(1),
		RunE: func(cmd *cobra.Command, args []string) error {
			clientCtx, err := client.GetClientQueryContext(cmd)
			if err != nil {
				return err
			}

			queryClient := types.NewQueryClient(clientCtx)

			res, err := queryClient.Contract(context.Background(), &types.QueryContractRequest{
				Address: args[0],
			})
			if err != nil {
				return err
			}

			return clientCtx.PrintProto(res)
		},
	}

	flags.AddQueryFlagsToCmd(cmd)

	return cmd
}

// CmdQueryContractState returns the query contract state command
func CmdQueryContractState() *cobra.Command {
	cmd := &cobra.Command{
		Use:   "contract-state [address]",
		Short: "Query the state of a contract",
		Long: `Query the raw state data of a deployed contract.

Example:
  aegisd query aegisvm contract-state aegis1abc...xyz
`,
		Args: cobra.ExactArgs(1),
		RunE: func(cmd *cobra.Command, args []string) error {
			clientCtx, err := client.GetClientQueryContext(cmd)
			if err != nil {
				return err
			}

			queryClient := types.NewQueryClient(clientCtx)

			res, err := queryClient.ContractState(context.Background(), &types.QueryContractStateRequest{
				Address: args[0],
			})
			if err != nil {
				return err
			}

			return clientCtx.PrintProto(res)
		},
	}

	flags.AddQueryFlagsToCmd(cmd)

	return cmd
}

// CmdQueryBytecode returns the query bytecode command
func CmdQueryBytecode() *cobra.Command {
	cmd := &cobra.Command{
		Use:   "bytecode [code-hash]",
		Short: "Query bytecode by code hash",
		Long: `Query the raw bytecode for a given code hash.

Example:
  aegisd query aegisvm bytecode 0x1234...abcd
`,
		Args: cobra.ExactArgs(1),
		RunE: func(cmd *cobra.Command, args []string) error {
			clientCtx, err := client.GetClientQueryContext(cmd)
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

			queryClient := types.NewQueryClient(clientCtx)

			res, err := queryClient.Bytecode(context.Background(), &types.QueryBytecodeRequest{
				CodeHash: codeHash,
			})
			if err != nil {
				return err
			}

			return clientCtx.PrintProto(res)
		},
	}

	flags.AddQueryFlagsToCmd(cmd)

	return cmd
}

// CmdQueryContracts returns the query all contracts command
func CmdQueryContracts() *cobra.Command {
	cmd := &cobra.Command{
		Use:   "contracts",
		Short: "Query all deployed contracts",
		Long: `Query a list of all deployed contracts.

Example:
  aegisd query aegisvm contracts
`,
		Args: cobra.NoArgs,
		RunE: func(cmd *cobra.Command, args []string) error {
			clientCtx, err := client.GetClientQueryContext(cmd)
			if err != nil {
				return err
			}

			queryClient := types.NewQueryClient(clientCtx)

			res, err := queryClient.Contracts(context.Background(), &types.QueryContractsRequest{})
			if err != nil {
				return err
			}

			return clientCtx.PrintProto(res)
		},
	}

	flags.AddQueryFlagsToCmd(cmd)

	return cmd
}

// CmdQueryIsCodeApproved returns the is-code-approved query command
func CmdQueryIsCodeApproved() *cobra.Command {
	cmd := &cobra.Command{
		Use:   "is-code-approved [code-hash]",
		Short: "Check if a code hash is approved for deployment",
		Long: `Check whether a bytecode hash has been approved by governance.

Example:
  aegisd query aegisvm is-code-approved 0x1234...abcd
`,
		Args: cobra.ExactArgs(1),
		RunE: func(cmd *cobra.Command, args []string) error {
			clientCtx, err := client.GetClientQueryContext(cmd)
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

			queryClient := types.NewQueryClient(clientCtx)

			res, err := queryClient.IsCodeApproved(context.Background(), &types.QueryIsCodeApprovedRequest{
				CodeHash: codeHash,
			})
			if err != nil {
				return err
			}

			return clientCtx.PrintProto(res)
		},
	}

	flags.AddQueryFlagsToCmd(cmd)

	return cmd
}

// CmdSimulateExecution returns the simulate execution command
func CmdSimulateExecution() *cobra.Command {
	cmd := &cobra.Command{
		Use:   "simulate [contract] [sender] [function] [args]",
		Short: "Simulate contract execution without state changes",
		Long: `Simulate executing a contract function without making state changes.
Useful for estimating gas and previewing return values.

Example:
  aegisd query aegisvm simulate aegis1abc... aegis1xyz... 0x12345678 0xdeadbeef
`,
		Args: cobra.MinimumNArgs(3),
		RunE: func(cmd *cobra.Command, args []string) error {
			clientCtx, err := client.GetClientQueryContext(cmd)
			if err != nil {
				return err
			}

			contractAddr := args[0]
			senderAddr := args[1]

			// Parse function selector
			funcStr := args[2]
			if len(funcStr) >= 2 && funcStr[:2] == "0x" {
				funcStr = funcStr[2:]
			}
			function, err := hex.DecodeString(funcStr)
			if err != nil {
				return fmt.Errorf("invalid function selector: %w", err)
			}

			// Parse arguments if provided
			var funcArgs []byte
			if len(args) > 3 {
				argsStr := args[3]
				if len(argsStr) >= 2 && argsStr[:2] == "0x" {
					argsStr = argsStr[2:]
				}
				funcArgs, err = hex.DecodeString(argsStr)
				if err != nil {
					return fmt.Errorf("invalid function arguments: %w", err)
				}
			}

			queryClient := types.NewQueryClient(clientCtx)

			res, err := queryClient.SimulateExecution(context.Background(), &types.QuerySimulateExecutionRequest{
				Contract: contractAddr,
				Sender:   senderAddr,
				Function: function,
				Args:     funcArgs,
			})
			if err != nil {
				return err
			}

			return clientCtx.PrintProto(res)
		},
	}

	flags.AddQueryFlagsToCmd(cmd)

	return cmd
}
