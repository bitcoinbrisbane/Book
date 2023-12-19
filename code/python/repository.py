#!/usr/bin/python3

class Repository:
    def __init__(self):
        self.accounts = {}

    def get_balance(self, account) -> int:
        return self.accounts[account]

    def set_balance(self, account, balance) -> None:
        self.accounts[account] = balance
