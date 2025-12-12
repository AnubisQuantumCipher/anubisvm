#!/usr/bin/env python3
"""Deploy the minimal ELF contract to AnubisVM."""

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
    # Load ELF hex
    with open('/Users/sicarii/anubisvm/contracts/test_elf/minimal_contract.hex', 'r') as f:
        elf_hex = f.read().strip()

    print(f"ELF size: {len(elf_hex) // 2} bytes")
    print(f"First 32 hex chars: {elf_hex[:32]}")
    print()

    # Deploy contract
    deploy_req = {
        "jsonrpc": "2.0",
        "method": "vm_deployContract",
        "params": {
            "elf_hex": elf_hex,
            "name": "NativeELFContract"
        },
        "id": 1
    }

    print("Deploying ELF contract...")
    response = send_request(deploy_req)
    print(f"Deploy response: {response}")
    print()

    # Parse response to get contract ID
    try:
        result = json.loads(response)
        if 'result' in result:
            # Extract contract_id from result
            result_str = result['result']
            if isinstance(result_str, str) and 'contract_id' in result_str:
                import re
                match = re.search(r'"contract_id":"([^"]+)"', result_str)
                if match:
                    contract_id = match.group(1)
                    print(f"Contract ID: {contract_id}")

                    # Try to invoke the contract
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

                    print("\nInvoking ELF contract (entry: main)...")
                    invoke_response = send_request(invoke_req)
                    print(f"Invoke response: {invoke_response}")
    except json.JSONDecodeError as e:
        print(f"Failed to parse response: {e}")

if __name__ == '__main__':
    main()
