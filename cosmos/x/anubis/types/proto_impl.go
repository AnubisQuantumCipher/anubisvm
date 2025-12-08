// Proto interface implementations for Cosmos SDK v0.50 compatibility
// These implement the gogoproto.Message interface for all types

package types

import "fmt"

// Params proto methods
func (m *Params) Reset()         { *m = Params{} }
func (m *Params) String() string { return fmt.Sprintf("%+v", *m) }
func (m *Params) ProtoMessage()  {}

// PrivateContract proto methods
func (m *PrivateContract) Reset()         { *m = PrivateContract{} }
func (m *PrivateContract) String() string { return fmt.Sprintf("%+v", *m) }
func (m *PrivateContract) ProtoMessage()  {}

// EncryptedCall proto methods
func (m *EncryptedCall) Reset()         { *m = EncryptedCall{} }
func (m *EncryptedCall) String() string { return fmt.Sprintf("%+v", *m) }
func (m *EncryptedCall) ProtoMessage()  {}

// EncryptedResult proto methods
func (m *EncryptedResult) Reset()         { *m = EncryptedResult{} }
func (m *EncryptedResult) String() string { return fmt.Sprintf("%+v", *m) }
func (m *EncryptedResult) ProtoMessage()  {}

// ZKStateProof proto methods
func (m *ZKStateProof) Reset()         { *m = ZKStateProof{} }
func (m *ZKStateProof) String() string { return fmt.Sprintf("%+v", *m) }
func (m *ZKStateProof) ProtoMessage()  {}

// SessionKey proto methods
func (m *SessionKey) Reset()         { *m = SessionKey{} }
func (m *SessionKey) String() string { return fmt.Sprintf("%+v", *m) }
func (m *SessionKey) ProtoMessage()  {}

// MsgPrivateExecuteResponse proto methods
func (m *MsgPrivateExecuteResponse) Reset()         { *m = MsgPrivateExecuteResponse{} }
func (m *MsgPrivateExecuteResponse) String() string { return fmt.Sprintf("%+v", *m) }
func (m *MsgPrivateExecuteResponse) ProtoMessage()  {}

// MsgEnablePrivateResponse proto methods
func (m *MsgEnablePrivateResponse) Reset()         { *m = MsgEnablePrivateResponse{} }
func (m *MsgEnablePrivateResponse) String() string { return "" }
func (m *MsgEnablePrivateResponse) ProtoMessage()  {}

// MsgSubmitProofResponse proto methods
func (m *MsgSubmitProofResponse) Reset()         { *m = MsgSubmitProofResponse{} }
func (m *MsgSubmitProofResponse) String() string { return fmt.Sprintf("%+v", *m) }
func (m *MsgSubmitProofResponse) ProtoMessage()  {}

// MsgInitSessionResponse proto methods
func (m *MsgInitSessionResponse) Reset()         { *m = MsgInitSessionResponse{} }
func (m *MsgInitSessionResponse) String() string { return fmt.Sprintf("%+v", *m) }
func (m *MsgInitSessionResponse) ProtoMessage()  {}

// MsgCloseSessionResponse proto methods
func (m *MsgCloseSessionResponse) Reset()         { *m = MsgCloseSessionResponse{} }
func (m *MsgCloseSessionResponse) String() string { return "" }
func (m *MsgCloseSessionResponse) ProtoMessage()  {}

// MsgSessionExecuteResponse proto methods
func (m *MsgSessionExecuteResponse) Reset()         { *m = MsgSessionExecuteResponse{} }
func (m *MsgSessionExecuteResponse) String() string { return fmt.Sprintf("%+v", *m) }
func (m *MsgSessionExecuteResponse) ProtoMessage()  {}

// Note: GenesisState is defined in module.go to avoid circular dependency
