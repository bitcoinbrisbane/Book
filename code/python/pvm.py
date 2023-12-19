#!/usr/bin/python3

# Dictionary of accounts and balances
accounts = {}


def get_balance(account) -> int:
    return accounts[account]


def transfer(_from, to, amount) -> None:
    if accounts[_from] < amount:
        raise ValueError("Insufficient funds")
    
    accounts[_from] -= amount
    accounts[to] += amount

