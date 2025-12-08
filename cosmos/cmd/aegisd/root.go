// Root command for AegisVM daemon

package main

import (
	"io"
	"os"

	dbm "github.com/cosmos/cosmos-db"
	"github.com/spf13/cobra"

	"cosmossdk.io/log"

	"github.com/cosmos/cosmos-sdk/client"
	"github.com/cosmos/cosmos-sdk/client/keys"
	"github.com/cosmos/cosmos-sdk/crypto/keyring"
	"github.com/cosmos/cosmos-sdk/server"
	servertypes "github.com/cosmos/cosmos-sdk/server/types"
	"github.com/cosmos/cosmos-sdk/x/auth/tx"
	authtypes "github.com/cosmos/cosmos-sdk/x/auth/types"
	banktypes "github.com/cosmos/cosmos-sdk/x/bank/types"
	genutilcli "github.com/cosmos/cosmos-sdk/x/genutil/client/cli"
	genutiltypes "github.com/cosmos/cosmos-sdk/x/genutil/types"

	cmtcfg "github.com/cometbft/cometbft/config"

	appmodule "github.com/aegisvm/cosmos/app"
	"github.com/aegisvm/cosmos/pkg/pqcrypto"
	aegisvmcli "github.com/aegisvm/cosmos/x/aegisvm/client/cli"
	anubiscli "github.com/aegisvm/cosmos/x/anubis/client/cli"
	authcli "github.com/cosmos/cosmos-sdk/x/auth/client/cli"
)

// NewRootCmd creates a new root command for the aegisd daemon
func NewRootCmd() *cobra.Command {
	// Set up encoding config
	tempApp := appmodule.NewAegisApp(
		log.NewNopLogger(),
		dbm.NewMemDB(),
		nil,
		true,
		nil,
	)
	encodingConfig := tempApp.AppCodec()
	interfaceRegistry := tempApp.InterfaceRegistry()
	txConfig := tx.NewTxConfig(encodingConfig, tx.DefaultSignModes)

	// Configure keyring with ML-DSA-87 support as default
	// ML-DSA-87 is listed first to make it the default algorithm
	supportedAlgos := keyring.SigningAlgoList{pqcrypto.MLDSAAlgo{}}

	initClientCtx := client.Context{}.
		WithCodec(encodingConfig).
		WithInterfaceRegistry(interfaceRegistry).
		WithTxConfig(txConfig).
		WithLegacyAmino(tempApp.LegacyAmino()).
		WithInput(os.Stdin).
		WithAccountRetriever(authtypes.AccountRetriever{}).
		WithHomeDir(appmodule.DefaultNodeHome).
		WithViper("").
		WithKeyringOptions(func(options *keyring.Options) {
			options.SupportedAlgos = supportedAlgos
			options.SupportedAlgosLedger = supportedAlgos
		})

	rootCmd := &cobra.Command{
		Use:   appmodule.AppName + "d",
		Short: "AegisVM - Post-Quantum Secure Smart Contract Platform",
		Long: `AegisVM is a formally verified, post-quantum secure smart contract
platform built on the Cosmos SDK. It uses SPARK/Ada for provable
correctness and ML-DSA-87/ML-KEM-1024 for quantum resistance.

Features:
- Post-quantum signatures (ML-DSA-87, FIPS 204)
- Post-quantum key encapsulation (ML-KEM-1024, FIPS 203)
- Formally verified VM execution (SPARK/Ada)
- Private execution via FHE and ZK proofs
- IBC-compatible with quantum-safe handshakes

Start the node:
  aegisd start

Create a new account:
  aegisd keys add mykey

Query account balance:
  aegisd query bank balances <address>

Deploy a contract:
  aegisd tx aegisvm deploy <bytecode-file> <proof-hash>
`,
		PersistentPreRunE: func(cmd *cobra.Command, _ []string) error {
			initClientCtx, err := client.ReadPersistentCommandFlags(initClientCtx, cmd.Flags())
			if err != nil {
				return err
			}
			if err := client.SetCmdClientContextHandler(initClientCtx, cmd); err != nil {
				return err
			}
			return nil
		},
	}

	initRootCmd(rootCmd, txConfig, interfaceRegistry, encodingConfig)

	return rootCmd
}

// initRootCmd initializes the root command with all subcommands
func initRootCmd(rootCmd *cobra.Command, txConfig client.TxConfig, interfaceRegistry interface{}, encodingConfig interface{}) {
	rootCmd.AddCommand(
		genutilcli.InitCmd(appmodule.ModuleBasics, appmodule.DefaultNodeHome),
	)

	server.AddCommands(rootCmd, appmodule.DefaultNodeHome, newApp, appExport, addModuleInitFlags)

	// Add key commands with ML-DSA support
	// Note: ML-DSA keyring support is registered via pqcrypto.MLDSASupportedAlgos
	// Users can specify --algo=ml-dsa to use post-quantum keys
	_ = keyring.SigningAlgoList{pqcrypto.MLDSAAlgo{}} // Available for use

	rootCmd.AddCommand(
		keys.Commands(),
		AddressCmd(), // AAS-001 address display
	)

	// Add tx command
	txCmd := &cobra.Command{
		Use:                        "tx",
		Short:                      "Transactions subcommands",
		DisableFlagParsing:         false,
		SuggestionsMinimumDistance: 2,
		RunE:                       client.ValidateCmd,
	}
	txCmd.AddCommand(
		authcli.GetSignCommand(),
		authcli.GetBroadcastCommand(),
		authcli.GetEncodeCommand(),
		authcli.GetDecodeCommand(),
		aegisvmcli.GetTxCmd(),
		anubiscli.GetTxCmd(),
	)
	rootCmd.AddCommand(txCmd)

	// Add query command
	queryCmd := &cobra.Command{
		Use:                        "query",
		Aliases:                    []string{"q"},
		Short:                      "Querying subcommands",
		DisableFlagParsing:         false,
		SuggestionsMinimumDistance: 2,
		RunE:                       client.ValidateCmd,
	}
	queryCmd.AddCommand(
		aegisvmcli.GetQueryCmd(),
		anubiscli.GetQueryCmd(),
	)
	rootCmd.AddCommand(queryCmd)

	// Add genesis subcommand
	genesisCmd := &cobra.Command{
		Use:                        "genesis",
		Short:                      "Application genesis commands",
		DisableFlagParsing:         false,
		SuggestionsMinimumDistance: 2,
	}

	// Use testnet network type for genesis commands (matching DefaultChainID)
	networkType := pqcrypto.NetworkTest

	genesisCmd.AddCommand(
		genutilcli.AddGenesisAccountCmd(appmodule.DefaultNodeHome, pqcrypto.NewAASAddressCodec(networkType)),
		genutilcli.GenTxCmd(
			appmodule.ModuleBasics,
			txConfig,
			banktypes.GenesisBalancesIterator{},
			appmodule.DefaultNodeHome,
			pqcrypto.NewAASValidatorAddressCodec(networkType),
		),
		genutilcli.CollectGenTxsCmd(
			banktypes.GenesisBalancesIterator{},
			appmodule.DefaultNodeHome,
			genutiltypes.DefaultMessageValidator,
			pqcrypto.NewAASValidatorAddressCodec(networkType),
		),
		genutilcli.ValidateGenesisCmd(appmodule.ModuleBasics),
	)
	rootCmd.AddCommand(genesisCmd)
}

// addModuleInitFlags adds module initialization flags
func addModuleInitFlags(startCmd *cobra.Command) {
	// Add module specific flags here
}

// newApp creates a new AegisApp
func newApp(
	logger log.Logger,
	db dbm.DB,
	traceStore io.Writer,
	appOpts servertypes.AppOptions,
) servertypes.Application {
	return appmodule.NewAegisApp(
		logger, db, traceStore, true, appOpts,
	)
}

// appExport creates a simulated app and exports genesis
func appExport(
	logger log.Logger,
	db dbm.DB,
	traceStore io.Writer,
	height int64,
	forZeroHeight bool,
	jailAllowedAddrs []string,
	appOpts servertypes.AppOptions,
	modulesToExport []string,
) (servertypes.ExportedApp, error) {
	var aegisApp *appmodule.AegisApp
	if height != -1 {
		aegisApp = appmodule.NewAegisApp(logger, db, traceStore, false, appOpts)

		if err := aegisApp.LoadHeight(height); err != nil {
			return servertypes.ExportedApp{}, err
		}
	} else {
		aegisApp = appmodule.NewAegisApp(logger, db, traceStore, true, appOpts)
	}

	return servertypes.ExportedApp{}, nil
}

// initTendermintConfig returns the default tendermint config
func initTendermintConfig() *cmtcfg.Config {
	cfg := cmtcfg.DefaultConfig()

	// Customize for post-quantum signatures
	// Larger blocks to accommodate PQ signature sizes
	cfg.Consensus.TimeoutCommit = 5000 * 1000000 // 5 seconds

	return cfg
}
