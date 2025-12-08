// Module definition for x/aegisvm

package aegisvm

import (
	"encoding/json"
	"fmt"

	"github.com/cosmos/cosmos-sdk/client"
	"github.com/cosmos/cosmos-sdk/codec"
	cdctypes "github.com/cosmos/cosmos-sdk/codec/types"
	sdk "github.com/cosmos/cosmos-sdk/types"
	"github.com/cosmos/cosmos-sdk/types/module"
	"github.com/grpc-ecosystem/grpc-gateway/runtime"
	"github.com/spf13/cobra"

	"github.com/aegisvm/cosmos/x/aegisvm/keeper"
	"github.com/aegisvm/cosmos/x/aegisvm/types"
)

var (
	_ module.AppModuleBasic = AppModuleBasic{}
)

// IsAppModule implements the appmodule.AppModule interface
func (am AppModule) IsAppModule() {}

// IsOnePerModuleType implements the depinject.OnePerModuleType interface
func (am AppModule) IsOnePerModuleType() {}

// AppModuleBasic implements the AppModuleBasic interface
type AppModuleBasic struct {
	cdc codec.BinaryCodec
}

// Name returns the module name
func (AppModuleBasic) Name() string {
	return types.ModuleName
}

// RegisterLegacyAminoCodec registers the module's types on the legacy amino codec
func (AppModuleBasic) RegisterLegacyAminoCodec(cdc *codec.LegacyAmino) {
	// Register message types
	cdc.RegisterConcrete(&types.MsgDeployContract{}, "aegisvm/DeployContract", nil)
	cdc.RegisterConcrete(&types.MsgExecuteContract{}, "aegisvm/ExecuteContract", nil)
	cdc.RegisterConcrete(&types.MsgMigrateContract{}, "aegisvm/MigrateContract", nil)
	cdc.RegisterConcrete(&types.MsgUpdateAdmin{}, "aegisvm/UpdateAdmin", nil)
}

// RegisterInterfaces registers the module's interface types
func (a AppModuleBasic) RegisterInterfaces(reg cdctypes.InterfaceRegistry) {
	reg.RegisterImplementations((*sdk.Msg)(nil),
		&types.MsgDeployContract{},
		&types.MsgExecuteContract{},
		&types.MsgMigrateContract{},
		&types.MsgUpdateAdmin{},
	)
}

// DefaultGenesis returns default genesis state
func (AppModuleBasic) DefaultGenesis(cdc codec.JSONCodec) json.RawMessage {
	// Use standard JSON encoding since GenesisState is a plain struct
	bz, err := json.Marshal(types.DefaultGenesisState())
	if err != nil {
		panic(fmt.Errorf("failed to marshal aegisvm genesis state: %w", err))
	}
	return bz
}

// ValidateGenesis validates genesis state
func (AppModuleBasic) ValidateGenesis(cdc codec.JSONCodec, config client.TxEncodingConfig, bz json.RawMessage) error {
	var gs types.GenesisState
	// Use standard JSON unmarshal since GenesisState is a plain struct
	if err := json.Unmarshal(bz, &gs); err != nil {
		return fmt.Errorf("failed to unmarshal %s genesis state: %w", types.ModuleName, err)
	}
	return types.ValidateGenesis(gs)
}

// RegisterGRPCGatewayRoutes registers the gRPC Gateway routes
func (AppModuleBasic) RegisterGRPCGatewayRoutes(clientCtx client.Context, mux *runtime.ServeMux) {
	// Register query routes here
}

// GetTxCmd returns the root tx command
func (a AppModuleBasic) GetTxCmd() *cobra.Command {
	return GetTxCmd()
}

// GetQueryCmd returns the root query command
func (AppModuleBasic) GetQueryCmd() *cobra.Command {
	return GetQueryCmd()
}

// AppModule implements the AppModule interface
type AppModule struct {
	AppModuleBasic
	keeper keeper.Keeper
}

// NewAppModule creates a new AppModule
func NewAppModule(cdc codec.Codec, keeper keeper.Keeper) AppModule {
	return AppModule{
		AppModuleBasic: AppModuleBasic{cdc: cdc},
		keeper:         keeper,
	}
}

// Name returns the module name
func (am AppModule) Name() string {
	return am.AppModuleBasic.Name()
}

// RegisterInvariants registers module invariants
func (am AppModule) RegisterInvariants(ir sdk.InvariantRegistry) {
	// Register invariants
}

// RegisterServices registers module services
func (am AppModule) RegisterServices(cfg module.Configurator) {
	types.RegisterMsgServer(cfg.MsgServer(), keeper.NewMsgServerImpl(am.keeper))
	types.RegisterQueryServer(cfg.QueryServer(), keeper.NewQueryServerImpl(am.keeper))
}

// InitGenesis initializes genesis state
func (am AppModule) InitGenesis(ctx sdk.Context, cdc codec.JSONCodec, gs json.RawMessage) {
	var genesisState types.GenesisState
	// Use standard JSON unmarshal since GenesisState is a plain struct
	if err := json.Unmarshal(gs, &genesisState); err != nil {
		panic(fmt.Errorf("failed to unmarshal aegisvm genesis: %w", err))
	}
	InitGenesis(ctx, am.keeper, genesisState)
}

// ExportGenesis exports genesis state
func (am AppModule) ExportGenesis(ctx sdk.Context, cdc codec.JSONCodec) json.RawMessage {
	gs := ExportGenesis(ctx, am.keeper)
	// Use standard JSON marshal since GenesisState is a plain struct
	bz, err := json.Marshal(gs)
	if err != nil {
		panic(fmt.Errorf("failed to marshal aegisvm genesis: %w", err))
	}
	return bz
}

// ConsensusVersion returns the consensus version
func (AppModule) ConsensusVersion() uint64 {
	return 1
}

// InitGenesis initializes the module's genesis state
func InitGenesis(ctx sdk.Context, k keeper.Keeper, gs types.GenesisState) {
	// Use sdk.Context directly with wrapped context for keeper methods
	goCtx := sdk.WrapSDKContext(ctx)
	if err := k.SetParams(goCtx, gs.Params); err != nil {
		panic(err)
	}

	// Initialize contracts from genesis
	for _, contract := range gs.Contracts {
		// Decode and store contracts
		// This is simplified - actual implementation would need proper decoding
		_ = contract
	}

	// Initialize approved codes
	for _, code := range gs.ApprovedCodes {
		// Decode and store approved codes
		_ = code
	}
}

// ExportGenesis exports the module's genesis state
func ExportGenesis(ctx sdk.Context, k keeper.Keeper) *types.GenesisState {
	goCtx := sdk.WrapSDKContext(ctx)
	params, err := k.GetParams(goCtx)
	if err != nil {
		panic(err)
	}

	var contracts []types.GenesisContract
	k.IterateContracts(goCtx, func(contract types.Contract) bool {
		gc := types.GenesisContract{
			Address:  sdk.AccAddress(contract.Address).String(),
			Creator:  sdk.AccAddress(contract.Creator).String(),
			Metadata: contract.Metadata,
		}
		contracts = append(contracts, gc)
		return false
	})

	return &types.GenesisState{
		Params:    params,
		Contracts: contracts,
	}
}

// NewHandler is deprecated in SDK v0.50+, using Configurator-based service registration instead

// GetTxCmd returns the root tx command for the module
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

// GetQueryCmd returns the root query command for the module
func GetQueryCmd() *cobra.Command {
	cmd := &cobra.Command{
		Use:                        types.ModuleName,
		Short:                      "AegisVM query subcommands",
		DisableFlagParsing:         true,
		SuggestionsMinimumDistance: 2,
		RunE:                       client.ValidateCmd,
	}

	cmd.AddCommand(
		CmdQueryParams(),
		CmdQueryContract(),
		CmdQueryContractState(),
		CmdQueryContracts(),
	)

	return cmd
}

// Placeholder command implementations
func CmdDeployContract() *cobra.Command {
	return &cobra.Command{
		Use:   "deploy [bytecode-file] [proof-hash]",
		Short: "Deploy a new contract",
		Args:  cobra.ExactArgs(2),
		RunE: func(cmd *cobra.Command, args []string) error {
			// Implementation
			return nil
		},
	}
}

func CmdExecuteContract() *cobra.Command {
	return &cobra.Command{
		Use:   "execute [contract] [function] [args]",
		Short: "Execute a contract function",
		Args:  cobra.MinimumNArgs(2),
		RunE: func(cmd *cobra.Command, args []string) error {
			// Implementation
			return nil
		},
	}
}

func CmdMigrateContract() *cobra.Command {
	return &cobra.Command{
		Use:   "migrate [contract] [new-bytecode-file] [new-proof-hash]",
		Short: "Migrate a contract to new bytecode",
		Args:  cobra.ExactArgs(3),
		RunE: func(cmd *cobra.Command, args []string) error {
			// Implementation
			return nil
		},
	}
}

func CmdUpdateAdmin() *cobra.Command {
	return &cobra.Command{
		Use:   "update-admin [contract] [new-admin]",
		Short: "Update contract admin",
		Args:  cobra.ExactArgs(2),
		RunE: func(cmd *cobra.Command, args []string) error {
			// Implementation
			return nil
		},
	}
}

func CmdQueryParams() *cobra.Command {
	return &cobra.Command{
		Use:   "params",
		Short: "Query module parameters",
		Args:  cobra.NoArgs,
		RunE: func(cmd *cobra.Command, args []string) error {
			// Implementation
			return nil
		},
	}
}

func CmdQueryContract() *cobra.Command {
	return &cobra.Command{
		Use:   "contract [address]",
		Short: "Query contract by address",
		Args:  cobra.ExactArgs(1),
		RunE: func(cmd *cobra.Command, args []string) error {
			// Implementation
			return nil
		},
	}
}

func CmdQueryContractState() *cobra.Command {
	return &cobra.Command{
		Use:   "contract-state [address]",
		Short: "Query contract state",
		Args:  cobra.ExactArgs(1),
		RunE: func(cmd *cobra.Command, args []string) error {
			// Implementation
			return nil
		},
	}
}

func CmdQueryContracts() *cobra.Command {
	return &cobra.Command{
		Use:   "contracts",
		Short: "Query all contracts",
		Args:  cobra.NoArgs,
		RunE: func(cmd *cobra.Command, args []string) error {
			// Implementation
			return nil
		},
	}
}
