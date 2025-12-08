// Proto interface implementations for Cosmos SDK v0.50 compatibility
// These implement the gogoproto.Message interface for all types

package types

import "fmt"

// GenesisState proto methods
func (m *GenesisState) Reset()         { *m = GenesisState{} }
func (m *GenesisState) String() string { return fmt.Sprintf("%+v", *m) }
func (m *GenesisState) ProtoMessage()  {}

// GenesisContract proto methods
func (m *GenesisContract) Reset()         { *m = GenesisContract{} }
func (m *GenesisContract) String() string { return fmt.Sprintf("%+v", *m) }
func (m *GenesisContract) ProtoMessage()  {}

// ApprovedCode proto methods
func (m *ApprovedCode) Reset()         { *m = ApprovedCode{} }
func (m *ApprovedCode) String() string { return fmt.Sprintf("%+v", *m) }
func (m *ApprovedCode) ProtoMessage()  {}

// MsgDeployContractResponse proto methods
func (m *MsgDeployContractResponse) Reset()         { *m = MsgDeployContractResponse{} }
func (m *MsgDeployContractResponse) String() string { return fmt.Sprintf("%+v", *m) }
func (m *MsgDeployContractResponse) ProtoMessage()  {}

// MsgExecuteContractResponse proto methods
func (m *MsgExecuteContractResponse) Reset()         { *m = MsgExecuteContractResponse{} }
func (m *MsgExecuteContractResponse) String() string { return fmt.Sprintf("%+v", *m) }
func (m *MsgExecuteContractResponse) ProtoMessage()  {}

// MsgMigrateContractResponse proto methods
func (m *MsgMigrateContractResponse) Reset()         { *m = MsgMigrateContractResponse{} }
func (m *MsgMigrateContractResponse) String() string { return fmt.Sprintf("%+v", *m) }
func (m *MsgMigrateContractResponse) ProtoMessage()  {}

// MsgUpdateAdminResponse proto methods
func (m *MsgUpdateAdminResponse) Reset()         { *m = MsgUpdateAdminResponse{} }
func (m *MsgUpdateAdminResponse) String() string { return "" }
func (m *MsgUpdateAdminResponse) ProtoMessage()  {}
