// AegisVM Cosmos Blockchain Daemon

package main

import (
	"os"

	"cosmossdk.io/log"

	svrcmd "github.com/cosmos/cosmos-sdk/server/cmd"

	"github.com/aegisvm/cosmos/app"
)

func main() {
	rootCmd := NewRootCmd()

	if err := svrcmd.Execute(rootCmd, "", app.DefaultNodeHome); err != nil {
		log.NewLogger(os.Stderr).Error("failure when running app", "err", err)
		os.Exit(1)
	}
}
