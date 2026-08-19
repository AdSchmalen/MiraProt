package main

import (
	"fmt"
	"net"
)

// FindFreePort scans TCP ports on 127.0.0.1 from startPort to maxPort and
// returns the first port that is available for binding.
func FindFreePort(startPort, maxPort int) (int, error) {
	for port := startPort; port <= maxPort; port++ {
		ln, err := net.Listen("tcp", fmt.Sprintf("127.0.0.1:%d", port))
		if err != nil {
			continue
		}
		_ = ln.Close()
		return port, nil
	}
	return 0, fmt.Errorf("no free port found in range %d-%d", startPort, maxPort)
}
