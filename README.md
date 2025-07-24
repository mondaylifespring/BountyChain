# BountyChain

A decentralized protocol for creating task bounties and rewarding contributors on the Stacks blockchain.

## Features

- Create bounties with customizable rewards and deadlines
- Stake bonds to claim bounties and ensure commitment
- Quality-based reward distribution system
- Multi-priority task categorization
- Transparent completion tracking

## Smart Contract Functions

### Public Functions
- `create-bounty` - Create a new task bounty
- `claim-bounty` - Claim a bounty with bond staking
- `update-quality-score` - Update work progress
- `issue-reward` - Complete bounty and receive reward
- `deactivate-bounty` - Deactivate bounty (creator only)

### Read-Only Functions
- `get-bounty` - Get bounty details
- `get-hunter-claim` - Get hunter's claim status
- `get-bounty-stats` - Get bounty statistics
- `get-protocol-stats` - Get protocol metrics

## Usage

Deploy the contract and start creating bounties for tasks, bug fixes, or development work. Contributors can claim bounties by staking bonds and earn rewards upon successful completion.

## License

MIT