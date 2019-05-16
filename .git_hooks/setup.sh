#!/usr/bin/env bash

# Setup the git hooks
echo "Setting up git-hooks"
echo "===================="

echo "Launched from" $(pwd)
echo ""

echo "Setting up post-commit"
ln -s -f ../../.git_hooks/post-commit ./.git/hooks/post-commit
chmod a+x ./.git/hooks/post-commit
echo "Done"

echo "Setting up post-merge"
ln -s -f ../../.git_hooks/post-merge ./.git/hooks/post-merge
chmod a+x ./.git/hooks/post-merge
echo "Done"

echo "Setting up post-checkout"
ln -s -f ../../.git_hooks/post-checkout ./.git/hooks/post-checkout
chmod a+x ./.git/hooks/post-checkout
echo "Done"

echo "Setting up pre-commit"
ln -s -f ../../.git_hooks/pre-commit ./.git/hooks/pre-commit
chmod a+x ./.git/hooks/pre-commit
echo "Done"
