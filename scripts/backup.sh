#!/bin/bash
# Anubis VM Backup Script
# Push to all configured remotes

set -e

cd "$(dirname "$0")/.."

echo "Backing up Anubis VM to all remotes..."

# Stage all changes
git add -A

# Check if there are changes to commit
if git diff --cached --quiet; then
    echo "No changes to commit."
else
    # Commit with timestamp
    git commit -m "backup: $(date -u +%Y-%m-%dT%H:%M:%SZ)

Pure Ada/SPARK Anubis VM
- ML-DSA-87 signatures (NIST FIPS 204)
- ML-KEM-1024 encryption (NIST FIPS 203)
- SPARK formally verified core
- Post-quantum ready"
fi

# Push to all configured remotes
echo "Pushing to origin..."
git push origin main --tags 2>/dev/null || git push origin master --tags 2>/dev/null || echo "Warning: Could not push to origin"

# Check for backup remote
if git remote | grep -q "backup"; then
    echo "Pushing to backup..."
    git push backup main --tags 2>/dev/null || git push backup master --tags 2>/dev/null || echo "Warning: Could not push to backup"
fi

# Check for AWS CodeCommit remote
if git remote | grep -q "aws"; then
    echo "Pushing to AWS..."
    git push aws main --tags 2>/dev/null || git push aws master --tags 2>/dev/null || echo "Warning: Could not push to aws"
fi

echo ""
echo "Backup complete."
git remote -v
