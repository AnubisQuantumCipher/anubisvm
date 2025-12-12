#!/usr/bin/env python3
"""Invoke the deployed ELF contract."""

import json
import socket

def send_request(request):
    """Send JSON-RPC request to the node."""
    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sock.connect(('localhost', 26659))
    sock.sendall(json.dumps(request).encode())
    sock.shutdown(socket.SHUT_WR)

    response = b''
    while True:
        chunk = sock.recv(4096)
        if not chunk:
            break
        response += chunk
    sock.close()
    return response.decode()

def main():
    # Contract ID from previous deploy
    contract_id = "mldsa87:dev:c:rz43bhmt-nkzdkj87-zv2g17dt-hr16mr9r-aaez7q4h-tvc33q79-8540-9139w"

    print(f"Invoking contract: {contract_id}")
    print()

    # Try to invoke the contract (this will trigger ELF loading)
    invoke_req = {
        "jsonrpc": "2.0",
        "method": "vm_invoke",
        "params": {
            "to": contract_id,
            "entry_point": "main",
            "args": "",
            "gas_limit": 100000
        },
        "id": 2
    }

    print("Calling entry point 'main'...")
    invoke_response = send_request(invoke_req)
    print(f"Response: {invoke_response}")

if __name__ == '__main__':
    main()
