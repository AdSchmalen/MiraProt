package main

import "errors"

var errAnotherInstance = errors.New("Another MiraProt instance is already running.")

// InstanceLock holds the operating-system resource that prevents two launcher
// processes from running in the same user session.
type InstanceLock interface {
	Acquire() error
	Release()
}
