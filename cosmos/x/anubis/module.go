// Module definition for x/anubis

package anubis

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

	"github.com/aegisvm/cosmos/x/anubis/keeper"
	"github.com/aegisvm/cosmos/x/anubis/types"
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
	cdc.RegisterConcrete(&types.MsgPrivateExecute{}, "anubis/PrivateExecute", nil)
	cdc.RegisterConcrete(&types.MsgEnablePrivate{}, "anubis/EnablePrivate", nil)
	cdc.RegisterConcrete(&types.MsgSubmitProof{}, "anubis/SubmitProof", nil)
	cdc.RegisterConcrete(&types.MsgInitSession{}, "anubis/InitSession", nil)
	cdc.RegisterConcrete(&types.MsgCloseSession{}, "anubis/CloseSession", nil)
	cdc.RegisterConcrete(&types.MsgSessionExecute{}, "anubis/SessionExecute", nil)
}

// RegisterInterfaces registers the module's interface types
func (a AppModuleBasic) RegisterInterfaces(reg cdctypes.InterfaceRegistry) {
	reg.RegisterImplementations((*sdk.Msg)(nil),
		&types.MsgPrivateExecute{},
		&types.MsgEnablePrivate{},
		&types.MsgSubmitProof{},
		&types.MsgInitSession{},
		&types.MsgCloseSession{},
		&types.MsgSessionExecute{},
	)
}

// DefaultGenesis returns default genesis state
func (AppModuleBasic) DefaultGenesis(cdc codec.JSONCodec) json.RawMessage {
	// Use standard JSON encoding since GenesisState is a plain struct
	bz, err := json.Marshal(DefaultGenesisState())
	if err != nil {
		panic(fmt.Errorf("failed to marshal anubis genesis state: %w", err))
	}
	return bz
}

// ValidateGenesis validates genesis state
func (AppModuleBasic) ValidateGenesis(cdc codec.JSONCodec, config client.TxEncodingConfig, bz json.RawMessage) error {
	var gs GenesisState
	// Use standard JSON unmarshal since GenesisState is a plain struct
	if err := json.Unmarshal(bz, &gs); err != nil {
		return fmt.Errorf("failed to unmarshal %s genesis state: %w", types.ModuleName, err)
	}
	return ValidateGenesis(gs)
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
}

// InitGenesis initializes genesis state
func (am AppModule) InitGenesis(ctx sdk.Context, cdc codec.JSONCodec, gs json.RawMessage) {
	var genesisState GenesisState
	// Use standard JSON unmarshal since GenesisState is a plain struct
	if err := json.Unmarshal(gs, &genesisState); err != nil {
		panic(fmt.Errorf("failed to unmarshal anubis genesis: %w", err))
	}
	InitGenesis(ctx, am.keeper, genesisState)
}

// ExportGenesis exports genesis state
func (am AppModule) ExportGenesis(ctx sdk.Context, cdc codec.JSONCodec) json.RawMessage {
	gs := ExportGenesis(ctx, am.keeper)
	// Use standard JSON marshal since GenesisState is a plain struct
	bz, err := json.Marshal(gs)
	if err != nil {
		panic(fmt.Errorf("failed to marshal anubis genesis: %w", err))
	}
	return bz
}

// ConsensusVersion returns the consensus version
func (AppModule) ConsensusVersion() uint64 {
	return 1
}

// Genesis state types
type GenesisState struct {
	Params           types.Params            `json:"params"`
	PrivateContracts []types.PrivateContract `json:"private_contracts,omitempty"`
}

// Proto interface implementations
func (m *GenesisState) Reset()         { *m = GenesisState{} }
func (m *GenesisState) String() string { return fmt.Sprintf("%+v", *m) }
func (m *GenesisState) ProtoMessage()  {}

// DefaultGenesisState returns the default genesis state
func DefaultGenesisState() *GenesisState {
	return &GenesisState{
		Params:           types.DefaultParams(),
		PrivateContracts: []types.PrivateContract{},
	}
}

// ValidateGenesis validates the genesis state
func ValidateGenesis(gs GenesisState) error {
	if err := gs.Params.Validate(); err != nil {
		return fmt.Errorf("invalid params: %w", err)
	}
	for i, pc := range gs.PrivateContracts {
		if err := types.ValidatePrivateContract(pc); err != nil {
			return fmt.Errorf("private contract %d: %w", i, err)
		}
	}
	return nil
}

// InitGenesis initializes the module's genesis state
func InitGenesis(ctx sdk.Context, k keeper.Keeper, gs GenesisState) {
	goCtx := sdk.WrapSDKContext(ctx)
	if err := k.SetParams(goCtx, gs.Params); err != nil {
		panic(err)
	}
	for _, pc := range gs.PrivateContracts {
		k.SetPrivateContract(goCtx, pc)
	}
}

// ExportGenesis exports the module's genesis state
func ExportGenesis(ctx sdk.Context, k keeper.Keeper) *GenesisState {
	goCtx := sdk.WrapSDKContext(ctx)
	params, err := k.GetParams(goCtx)
	if err != nil {
		panic(err)
	}
	return &GenesisState{
		Params: params,
	}
}

// GetTxCmd returns the root tx command for the module
func GetTxCmd() *cobra.Command {
	cmd := &cobra.Command{
		Use:                        types.ModuleName,
		Short:                      "Anubis privacy transaction subcommands",
		DisableFlagParsing:         true,
		SuggestionsMinimumDistance: 2,
		RunE:                       client.ValidateCmd,
	}

	cmd.AddCommand(
		CmdPrivateExecute(),
		CmdEnablePrivate(),
		CmdInitSession(),
		CmdCloseSession(),
	)

	return cmd
}

// GetQueryCmd returns the root query command for the module
func GetQueryCmd() *cobra.Command {
	cmd := &cobra.Command{
		Use:                        types.ModuleName,
		Short:                      "Anubis privacy query subcommands",
		DisableFlagParsing:         true,
		SuggestionsMinimumDistance: 2,
		RunE:                       client.ValidateCmd,
	}

	cmd.AddCommand(
		CmdQueryParams(),
		CmdQueryPrivateContract(),
		CmdQuerySession(),
	)

	return cmd
}

// Placeholder command implementations
func CmdPrivateExecute() *cobra.Command {
	return &cobra.Command{
		Use:   "execute [contract] [encrypted-data]",
		Short: "Execute a contract privately",
		Args:  cobra.ExactArgs(2),
		RunE: func(cmd *cobra.Command, args []string) error {
			return nil
		},
	}
}

func CmdEnablePrivate() *cobra.Command {
	return &cobra.Command{
		Use:   "enable [contract] [public-key-file]",
		Short: "Enable private execution for a contract",
		Args:  cobra.ExactArgs(2),
		RunE: func(cmd *cobra.Command, args []string) error {
			return nil
		},
	}
}

func CmdInitSession() *cobra.Command {
	return &cobra.Command{
		Use:   "init-session [contract]",
		Short: "Initialize a private execution session",
		Args:  cobra.ExactArgs(1),
		RunE: func(cmd *cobra.Command, args []string) error {
			return nil
		},
	}
}

func CmdCloseSession() *cobra.Command {
	return &cobra.Command{
		Use:   "close-session [session-id]",
		Short: "Close a private execution session",
		Args:  cobra.ExactArgs(1),
		RunE: func(cmd *cobra.Command, args []string) error {
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
			return nil
		},
	}
}

func CmdQueryPrivateContract() *cobra.Command {
	return &cobra.Command{
		Use:   "private-contract [address]",
		Short: "Query private contract configuration",
		Args:  cobra.ExactArgs(1),
		RunE: func(cmd *cobra.Command, args []string) error {
			return nil
		},
	}
}

func CmdQuerySession() *cobra.Command {
	return &cobra.Command{
		Use:   "session [session-id]",
		Short: "Query session details",
		Args:  cobra.ExactArgs(1),
		RunE: func(cmd *cobra.Command, args []string) error {
			return nil
		},
	}
}
