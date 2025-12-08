// AegisVM Cosmos Application
//
// This is a simplified app structure for Cosmos SDK v0.50+
// that integrates the AegisVM and Anubis modules.

package app

import (
	"encoding/json"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strings"

	dbm "github.com/cosmos/cosmos-db"

	"cosmossdk.io/log"
	storetypes "cosmossdk.io/store/types"
	txsigning "cosmossdk.io/x/tx/signing"
	"cosmossdk.io/x/tx/signing/aminojson"

	"google.golang.org/protobuf/reflect/protoregistry"

	"github.com/cosmos/cosmos-sdk/baseapp"
	"github.com/cosmos/cosmos-sdk/client"
	"github.com/cosmos/cosmos-sdk/codec"
	codectypes "github.com/cosmos/cosmos-sdk/codec/types"
	"github.com/cosmos/cosmos-sdk/runtime"
	"github.com/cosmos/cosmos-sdk/server/api"
	"github.com/cosmos/cosmos-sdk/server/config"
	servertypes "github.com/cosmos/cosmos-sdk/server/types"
	"github.com/cosmos/cosmos-sdk/std"
	sdk "github.com/cosmos/cosmos-sdk/types"
	"github.com/cosmos/cosmos-sdk/types/module"
	"github.com/cosmos/cosmos-sdk/x/auth"
	sdkante "github.com/cosmos/cosmos-sdk/x/auth/ante"
	authkeeper "github.com/cosmos/cosmos-sdk/x/auth/keeper"
	authtypes "github.com/cosmos/cosmos-sdk/x/auth/types"
	"github.com/cosmos/cosmos-sdk/x/auth/tx"
	"github.com/cosmos/cosmos-sdk/x/bank"
	bankkeeper "github.com/cosmos/cosmos-sdk/x/bank/keeper"
	banktypes "github.com/cosmos/cosmos-sdk/x/bank/types"
	"github.com/cosmos/cosmos-sdk/x/consensus"
	consensuskeeper "github.com/cosmos/cosmos-sdk/x/consensus/keeper"
	consensustypes "github.com/cosmos/cosmos-sdk/x/consensus/types"
	"github.com/cosmos/cosmos-sdk/x/staking"
	stakingkeeper "github.com/cosmos/cosmos-sdk/x/staking/keeper"
	stakingtypes "github.com/cosmos/cosmos-sdk/x/staking/types"
	"github.com/cosmos/cosmos-sdk/x/genutil"
	genutiltypes "github.com/cosmos/cosmos-sdk/x/genutil/types"

	abci "github.com/cometbft/cometbft/abci/types"

	"github.com/aegisvm/cosmos/app/ante"
	"github.com/aegisvm/cosmos/pkg/pqcrypto"
	"github.com/aegisvm/cosmos/x/aegisvm"
	aegisvmkeeper "github.com/aegisvm/cosmos/x/aegisvm/keeper"
	aegisvmtypes "github.com/aegisvm/cosmos/x/aegisvm/types"
	"github.com/aegisvm/cosmos/x/anubis"
	anubiskeeper "github.com/aegisvm/cosmos/x/anubis/keeper"
	anubistypes "github.com/aegisvm/cosmos/x/anubis/types"
)

const (
	AppName = "aegisvm"

	// Bech32 prefixes
	Bech32MainPrefix = "aegis"

	// Token denom
	BaseDenom = "uaegis"

	// Default chain ID for testnet
	DefaultChainID = "aegis-testnet-1"
)

var (
	// DefaultNodeHome is the default home directory for the application daemon
	DefaultNodeHome string

	// ModuleBasics defines the module BasicManager
	ModuleBasics = module.NewBasicManager(
		auth.AppModuleBasic{},
		bank.AppModuleBasic{},
		staking.AppModuleBasic{},
		consensus.AppModuleBasic{},
		genutil.NewAppModuleBasic(genutiltypes.DefaultMessageValidator),
		aegisvm.AppModuleBasic{},
		anubis.AppModuleBasic{},
	)

	// Module account permissions
	maccPerms = map[string][]string{
		authtypes.FeeCollectorName:     nil,
		stakingtypes.BondedPoolName:    {authtypes.Burner, authtypes.Staking},
		stakingtypes.NotBondedPoolName: {authtypes.Burner, authtypes.Staking},
		aegisvmtypes.ModuleName:        {authtypes.Minter, authtypes.Burner},
		anubistypes.ModuleName:         nil,
	}
)

func init() {
	userHomeDir, err := os.UserHomeDir()
	if err != nil {
		panic(err)
	}
	DefaultNodeHome = filepath.Join(userHomeDir, ".aegisd")

	// Set Bech32 prefixes
	cfg := sdk.GetConfig()
	cfg.SetBech32PrefixForAccount(Bech32MainPrefix, Bech32MainPrefix+"pub")
	cfg.SetBech32PrefixForValidator(Bech32MainPrefix+"valoper", Bech32MainPrefix+"valoperpub")
	cfg.SetBech32PrefixForConsensusNode(Bech32MainPrefix+"valcons", Bech32MainPrefix+"valconspub")
	cfg.Seal()
}

// AegisApp extends an ABCI application
type AegisApp struct {
	*baseapp.BaseApp

	legacyAmino       *codec.LegacyAmino
	appCodec          codec.Codec
	txConfig          client.TxConfig
	interfaceRegistry codectypes.InterfaceRegistry

	// Store keys
	keys    map[string]*storetypes.KVStoreKey
	tkeys   map[string]*storetypes.TransientStoreKey
	memKeys map[string]*storetypes.MemoryStoreKey

	// Keepers
	AccountKeeper   authkeeper.AccountKeeper
	BankKeeper      bankkeeper.Keeper
	StakingKeeper   *stakingkeeper.Keeper
	ConsensusKeeper consensuskeeper.Keeper

	// Custom module keepers
	AegisVMKeeper aegisvmkeeper.Keeper
	AnubisKeeper  anubiskeeper.Keeper

	// Module manager
	ModuleManager *module.Manager
}

// NewAegisApp creates a new AegisApp
func NewAegisApp(
	logger log.Logger,
	db dbm.DB,
	traceStore io.Writer,
	loadLatest bool,
	appOpts servertypes.AppOptions,
	baseAppOptions ...func(*baseapp.BaseApp),
) *AegisApp {
	// Determine network type from chain ID
	networkType := pqcrypto.NetworkMain
	if strings.Contains(DefaultChainID, "test") {
		networkType = pqcrypto.NetworkTest
	} else if strings.Contains(DefaultChainID, "dev") {
		networkType = pqcrypto.NetworkDev
	}

	// Create signing options with AAS-001 address codecs
	signingOpts := txsigning.Options{
		AddressCodec:          pqcrypto.NewAASAddressCodec(networkType),
		ValidatorAddressCodec: pqcrypto.NewAASValidatorAddressCodec(networkType),
	}

	// Create interface registry with address codecs and codec
	interfaceRegistry, err := codectypes.NewInterfaceRegistryWithOptions(codectypes.InterfaceRegistryOptions{
		ProtoFiles:     protoregistry.GlobalFiles,
		SigningOptions: signingOpts,
	})
	if err != nil {
		panic(fmt.Errorf("failed to create interface registry: %w", err))
	}
	appCodec := codec.NewProtoCodec(interfaceRegistry)
	legacyAmino := codec.NewLegacyAmino()
	_ = aminojson.SignModeHandler{} // Ensure import is used

	// Register standard types
	std.RegisterLegacyAminoCodec(legacyAmino)
	std.RegisterInterfaces(interfaceRegistry)

	// Register PQ crypto types for ML-DSA-87
	pqcrypto.RegisterLegacyAminoCodec(legacyAmino)
	pqcrypto.RegisterInterfaces(interfaceRegistry)

	// Register all module interfaces
	authtypes.RegisterInterfaces(interfaceRegistry)
	banktypes.RegisterInterfaces(interfaceRegistry)
	stakingtypes.RegisterInterfaces(interfaceRegistry)
	consensustypes.RegisterInterfaces(interfaceRegistry)

	// Create tx config
	txConfig := tx.NewTxConfig(appCodec, tx.DefaultSignModes)

	// Create base app with chain ID
	allOptions := append(baseAppOptions, baseapp.SetChainID(DefaultChainID))
	bApp := baseapp.NewBaseApp(AppName, logger, db, txConfig.TxDecoder(), allOptions...)
	bApp.SetCommitMultiStoreTracer(traceStore)
	bApp.SetVersion(Version)
	bApp.SetInterfaceRegistry(interfaceRegistry)
	bApp.SetTxEncoder(txConfig.TxEncoder())

	// Create store keys
	keys := storetypes.NewKVStoreKeys(
		authtypes.StoreKey,
		banktypes.StoreKey,
		stakingtypes.StoreKey,
		consensustypes.StoreKey,
		aegisvmtypes.StoreKey,
		anubistypes.StoreKey,
	)
	tkeys := storetypes.NewTransientStoreKeys()
	memKeys := storetypes.NewMemoryStoreKeys(aegisvmtypes.MemStoreKey, anubistypes.MemStoreKey)

	app := &AegisApp{
		BaseApp:           bApp,
		legacyAmino:       legacyAmino,
		appCodec:          appCodec,
		txConfig:          txConfig,
		interfaceRegistry: interfaceRegistry,
		keys:              keys,
		tkeys:             tkeys,
		memKeys:           memKeys,
	}

	// Authority address for governance - convert using AAS-001 codec
	addressCodec := pqcrypto.NewAASAddressCodec(networkType)
	authority, err := addressCodec.BytesToString(authtypes.NewModuleAddress("gov"))
	if err != nil {
		panic(fmt.Errorf("failed to format authority address: %w", err))
	}

	// Initialize consensus keeper (required for chain ID storage)
	app.ConsensusKeeper = consensuskeeper.NewKeeper(
		appCodec,
		runtime.NewKVStoreService(keys[consensustypes.StoreKey]),
		authority,
		runtime.EventService{},
	)
	bApp.SetParamStore(app.ConsensusKeeper.ParamsStore)

	// Initialize AccountKeeper with AAS-001 address codec
	app.AccountKeeper = authkeeper.NewAccountKeeper(
		appCodec,
		runtime.NewKVStoreService(keys[authtypes.StoreKey]),
		authtypes.ProtoBaseAccount,
		maccPerms,
		pqcrypto.NewAASAddressCodec(networkType),
		Bech32MainPrefix, // Legacy prefix for compatibility
		authority,
	)

	// Initialize BankKeeper
	app.BankKeeper = bankkeeper.NewBaseKeeper(
		appCodec,
		runtime.NewKVStoreService(keys[banktypes.StoreKey]),
		app.AccountKeeper,
		BlockedAddresses(),
		authority,
		logger,
	)

	// Initialize StakingKeeper with AAS-001 address codecs
	app.StakingKeeper = stakingkeeper.NewKeeper(
		appCodec,
		runtime.NewKVStoreService(keys[stakingtypes.StoreKey]),
		app.AccountKeeper,
		app.BankKeeper,
		authority,
		pqcrypto.NewAASValidatorAddressCodec(networkType),
		pqcrypto.NewAASConsensusAddressCodec(networkType),
	)

	// Initialize AegisVM keeper
	app.AegisVMKeeper = aegisvmkeeper.NewKeeper(
		appCodec,
		keys[aegisvmtypes.StoreKey],
		memKeys[aegisvmtypes.MemStoreKey],
		app.BankKeeper,
		authority,
	)

	// Initialize Anubis keeper
	app.AnubisKeeper = anubiskeeper.NewKeeper(
		appCodec,
		keys[anubistypes.StoreKey],
		memKeys[anubistypes.MemStoreKey],
		app.AegisVMKeeper,
		app.BankKeeper,
		authority,
	)

	// Create module manager
	app.ModuleManager = module.NewManager(
		auth.NewAppModule(appCodec, app.AccountKeeper, nil, nil),
		bank.NewAppModule(appCodec, app.BankKeeper, app.AccountKeeper, nil),
		staking.NewAppModule(appCodec, app.StakingKeeper, app.AccountKeeper, app.BankKeeper, nil),
		consensus.NewAppModule(appCodec, app.ConsensusKeeper),
		genutil.NewAppModule(app.AccountKeeper, app.StakingKeeper, app, txConfig),
		aegisvm.NewAppModule(appCodec, app.AegisVMKeeper),
		anubis.NewAppModule(appCodec, app.AnubisKeeper),
	)

	// Set order for genesis and begin/end blockers
	// Order matters: auth must be first, then bank, then staking
	app.ModuleManager.SetOrderInitGenesis(
		authtypes.ModuleName,
		banktypes.ModuleName,
		stakingtypes.ModuleName,
		genutiltypes.ModuleName, // genutil must be after staking to process gentxs
		consensustypes.ModuleName,
		aegisvmtypes.ModuleName,
		anubistypes.ModuleName,
	)

	// Set order for begin blockers
	app.ModuleManager.SetOrderBeginBlockers(
		stakingtypes.ModuleName,
		authtypes.ModuleName,
		banktypes.ModuleName,
		consensustypes.ModuleName,
		aegisvmtypes.ModuleName,
		anubistypes.ModuleName,
	)

	// Set order for end blockers
	app.ModuleManager.SetOrderEndBlockers(
		stakingtypes.ModuleName,
		authtypes.ModuleName,
		banktypes.ModuleName,
		consensustypes.ModuleName,
		aegisvmtypes.ModuleName,
		anubistypes.ModuleName,
	)

	// Register services
	app.ModuleManager.RegisterServices(module.NewConfigurator(appCodec, app.MsgServiceRouter(), app.GRPCQueryRouter()))

	// Mount stores
	app.MountKVStores(keys)
	app.MountTransientStores(tkeys)
	app.MountMemoryStores(memKeys)

	// Initialize base app
	app.SetInitChainer(app.InitChainer)

	// Set up ante handler with PQ signature support
	anteHandler, err := sdkante.NewAnteHandler(
		sdkante.HandlerOptions{
			AccountKeeper:   app.AccountKeeper,
			BankKeeper:      app.BankKeeper,
			SignModeHandler: txConfig.SignModeHandler(),
			FeegrantKeeper:  nil,
			SigGasConsumer:  ante.DefaultSigVerificationGasConsumer, // Custom gas consumer for PQ signatures
		},
	)
	if err != nil {
		panic(fmt.Errorf("failed to create ante handler: %w", err))
	}
	app.SetAnteHandler(anteHandler)

	if loadLatest {
		if err := app.LoadLatestVersion(); err != nil {
			panic(err)
		}
	}

	return app
}

// InitChainer application update at chain initialization
func (app *AegisApp) InitChainer(ctx sdk.Context, req *abci.RequestInitChain) (*abci.ResponseInitChain, error) {
	var genesisState map[string]json.RawMessage
	if err := json.Unmarshal(req.AppStateBytes, &genesisState); err != nil {
		panic(err)
	}

	// Initialize modules - consensus module will handle params storage
	resp, err := app.ModuleManager.InitGenesis(ctx, app.appCodec, genesisState)
	if err != nil {
		return nil, err
	}

	return resp, nil
}

// Name returns the name of the App
func (app *AegisApp) Name() string { return app.BaseApp.Name() }

// LegacyAmino returns the application's legacy amino codec
func (app *AegisApp) LegacyAmino() *codec.LegacyAmino {
	return app.legacyAmino
}

// AppCodec returns the application's codec
func (app *AegisApp) AppCodec() codec.Codec {
	return app.appCodec
}

// InterfaceRegistry returns the application's interface registry
func (app *AegisApp) InterfaceRegistry() codectypes.InterfaceRegistry {
	return app.interfaceRegistry
}

// TxConfig returns the application's TxConfig
func (app *AegisApp) TxConfig() client.TxConfig {
	return app.txConfig
}

// RegisterAPIRoutes registers all application module routes with the provided API server
func (app *AegisApp) RegisterAPIRoutes(apiSvr *api.Server, apiConfig config.APIConfig) {
	clientCtx := apiSvr.ClientCtx
	ModuleBasics.RegisterGRPCGatewayRoutes(clientCtx, apiSvr.GRPCGatewayRouter)
}

// RegisterTxService implements the Application.RegisterTxService method
func (app *AegisApp) RegisterTxService(clientCtx client.Context) {
	// Register transaction service
}

// RegisterTendermintService implements the Application.RegisterTendermintService method
func (app *AegisApp) RegisterTendermintService(clientCtx client.Context) {
	// Register CometBFT service
}

// RegisterNodeService implements the Application.RegisterNodeService method
func (app *AegisApp) RegisterNodeService(clientCtx client.Context, cfg config.Config) {
	// Register node service
}

// FinalizeBlock implements the ABCI FinalizeBlock method
func (app *AegisApp) FinalizeBlock(req *abci.RequestFinalizeBlock) (*abci.ResponseFinalizeBlock, error) {
	return app.BaseApp.FinalizeBlock(req)
}

// DefaultGenesis returns a default genesis from the registered modules
func (app *AegisApp) DefaultGenesis() map[string]json.RawMessage {
	return ModuleBasics.DefaultGenesis(app.appCodec)
}

// LoadHeight loads a particular height
func (app *AegisApp) LoadHeight(height int64) error {
	return app.LoadVersion(height)
}

var (
	// Version is the application version
	Version = "0.1.0"
)

// BlockedAddresses returns all the module account addresses that are blocked from receiving funds
func BlockedAddresses() map[string]bool {
	blockedAddrs := make(map[string]bool)
	for acc := range maccPerms {
		blockedAddrs[authtypes.NewModuleAddress(acc).String()] = true
	}
	return blockedAddrs
}
