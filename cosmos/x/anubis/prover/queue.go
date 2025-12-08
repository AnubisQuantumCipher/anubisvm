// Queue management for proof requests

package prover

import (
	"errors"
	"sync"
)

// ErrQueueFull is returned when the queue is at capacity
var ErrQueueFull = errors.New("queue full")

// ProofQueue manages pending proof requests
type ProofQueue struct {
	mu       sync.Mutex
	requests []*PrivateExecutionRequest
	maxSize  int
}

// NewProofQueue creates a new proof queue
func NewProofQueue(maxSize int) *ProofQueue {
	return &ProofQueue{
		requests: make([]*PrivateExecutionRequest, 0, maxSize),
		maxSize:  maxSize,
	}
}

// Push adds a request to the queue
func (q *ProofQueue) Push(request *PrivateExecutionRequest) error {
	q.mu.Lock()
	defer q.mu.Unlock()

	if len(q.requests) >= q.maxSize {
		return ErrQueueFull
	}

	q.requests = append(q.requests, request)
	return nil
}

// Pop removes and returns the highest priority request
func (q *ProofQueue) Pop() *PrivateExecutionRequest {
	q.mu.Lock()
	defer q.mu.Unlock()

	if len(q.requests) == 0 {
		return nil
	}

	// Find highest priority (closest deadline)
	bestIdx := 0
	for i := 1; i < len(q.requests); i++ {
		if q.requests[i].Deadline < q.requests[bestIdx].Deadline {
			bestIdx = i
		}
	}

	request := q.requests[bestIdx]
	// Remove from queue
	q.requests = append(q.requests[:bestIdx], q.requests[bestIdx+1:]...)
	return request
}

// Len returns the current queue size
func (q *ProofQueue) Len() int {
	q.mu.Lock()
	defer q.mu.Unlock()
	return len(q.requests)
}

// Clear empties the queue
func (q *ProofQueue) Clear() {
	q.mu.Lock()
	defer q.mu.Unlock()
	q.requests = q.requests[:0]
}
