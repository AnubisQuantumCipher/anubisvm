// Query CLI commands for x/anubis

package cli

import (
	"context"
	"encoding/hex"
	"fmt"

	"github.com/cosmos/cosmos-sdk/client"
	"github.com/cosmos/cosmos-sdk/client/flags"
	"github.com/spf13/cobra"

	"github.com/aegisvm/cosmos/x/anubis/types"
)

// GetQueryCmd returns the query commands for this module
func GetQueryCmd() *cobra.Command {
	cmd := &cobra.Command{
		Use:                        types.ModuleName,
		Short:                      "Querying commands for the anubis module",
		DisableFlagParsing:         true,
		SuggestionsMinimumDistance: 2,
		RunE:                       client.ValidateCmd,
	}

	cmd.AddCommand(
		CmdQueryParams(),
		CmdQueryPrivateContract(),
		CmdQuerySession(),
		CmdQueryEncryptedCall(),
		CmdQueryZKProof(),
	)

	return cmd
}

// CmdQueryParams returns the query params command
func CmdQueryParams() *cobra.Command {
	cmd := &cobra.Command{
		Use:   "params",
		Short: "Query the current module parameters",
		Long: `Query the current configuration parameters for the anubis privacy module.

Example:
  aegisd query anubis params
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

// CmdQueryPrivateContract returns the query private contract command
func CmdQueryPrivateContract() *cobra.Command {
	cmd := &cobra.Command{
		Use:   "private-contract [address]",
		Short: "Query private execution configuration for a contract",
		Long: `Query the private execution settings for a contract,
including its public key and authorized callers.

Example:
  aegisd query anubis private-contract aegis1abc...xyz
`,
		Args: cobra.ExactArgs(1),
		RunE: func(cmd *cobra.Command, args []string) error {
			clientCtx, err := client.GetClientQueryContext(cmd)
			if err != nil {
				return err
			}

			queryClient := types.NewQueryClient(clientCtx)

			res, err := queryClient.PrivateContract(context.Background(), &types.QueryPrivateContractRequest{
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

// CmdQuerySession returns the query session command
func CmdQuerySession() *cobra.Command {
	cmd := &cobra.Command{
		Use:   "session [session-id]",
		Short: "Query session details",
		Long: `Query information about an active private execution session.

Example:
  aegisd query anubis session 0x1234...abcd
`,
		Args: cobra.ExactArgs(1),
		RunE: func(cmd *cobra.Command, args []string) error {
			clientCtx, err := client.GetClientQueryContext(cmd)
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

			queryClient := types.NewQueryClient(clientCtx)

			res, err := queryClient.Session(context.Background(), &types.QuerySessionRequest{
				SessionId: sessionID,
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

// CmdQueryEncryptedCall returns the query encrypted call command
func CmdQueryEncryptedCall() *cobra.Command {
	cmd := &cobra.Command{
		Use:   "encrypted-call [call-id]",
		Short: "Query a pending encrypted call",
		Long: `Query information about a pending encrypted call awaiting proof submission.

Example:
  aegisd query anubis encrypted-call 0x5678...ef
`,
		Args: cobra.ExactArgs(1),
		RunE: func(cmd *cobra.Command, args []string) error {
			clientCtx, err := client.GetClientQueryContext(cmd)
			if err != nil {
				return err
			}

			// Parse call ID
			callIDStr := args[0]
			if len(callIDStr) >= 2 && callIDStr[:2] == "0x" {
				callIDStr = callIDStr[2:]
			}
			callID, err := hex.DecodeString(callIDStr)
			if err != nil {
				return fmt.Errorf("invalid call ID: %w", err)
			}

			queryClient := types.NewQueryClient(clientCtx)

			res, err := queryClient.EncryptedCall(context.Background(), &types.QueryEncryptedCallRequest{
				CallId: callID,
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

// CmdQueryZKProof returns the query ZK proof command
func CmdQueryZKProof() *cobra.Command {
	cmd := &cobra.Command{
		Use:   "zk-proof [proof-hash]",
		Short: "Query a submitted ZK proof",
		Long: `Query details of a submitted zero-knowledge proof.

Example:
  aegisd query anubis zk-proof 0xabcd...1234
`,
		Args: cobra.ExactArgs(1),
		RunE: func(cmd *cobra.Command, args []string) error {
			clientCtx, err := client.GetClientQueryContext(cmd)
			if err != nil {
				return err
			}

			// Parse proof hash
			proofHashStr := args[0]
			if len(proofHashStr) >= 2 && proofHashStr[:2] == "0x" {
				proofHashStr = proofHashStr[2:]
			}
			proofHash, err := hex.DecodeString(proofHashStr)
			if err != nil {
				return fmt.Errorf("invalid proof hash: %w", err)
			}

			queryClient := types.NewQueryClient(clientCtx)

			res, err := queryClient.ZKProof(context.Background(), &types.QueryZKProofRequest{
				ProofHash: proofHash,
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
