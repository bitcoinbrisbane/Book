#!/usr/bin/python3

# Dictionary of accounts and balances
accounts = {}


def get_balance(account) -> int:
    return accounts[account]


def set_balance(account, balance) -> None:
    accounts[account] = balance
