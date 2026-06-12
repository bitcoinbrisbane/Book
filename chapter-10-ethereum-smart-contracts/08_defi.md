## DeFi — Decentralised Finance

Some of these token concepts have been borrowed wholesale by popular DeFi (decentralised finance) protocols. During 2021, DeFi was blowing up and effectively taking over Ethereum. Standards like ERC20, ERC777, and ERC4626 allowed developers to compose protocols together like **lego blocks** — one contract's output became another's input, and entire financial primitives stacked on top of each other inside a single transaction.

There are lots of great DeFi protocols out there, but on the next few pages I'll talk through a couple of the most popular ones and the mechanics that make them work.

## Aave — lending

[Aave](https://aave.com/) is a popular lending protocol. Users deposit capital and receive a yield (interest) in return — much like a bank sources deposits from its customers. With more deposits, the protocol can lend out more tokens, and earns revenue on the **spread** between the interest paid on deposits and the interest charged on loans.

The concept is as old as banking itself. Unlike today's banks, however, the protocol *cannot be fractional*. Every token loaned out is fully backed by a deposit somewhere in the pool; there is no reserve banking under the hood, because every transfer is on-chain and verifiable. You can read the balance of the entire pool from any block-explorer.

## Uniswap — automated market making

[Uniswap](https://uniswap.org/) is an **automated market maker**, or AMM. Users can swap one ERC20 token for another — say USDC for wrapped Bitcoin — through a swap smart contract. Because every token on the platform adheres to the same ERC20 interface, the Uniswap protocol (and dozens of others built on top of it) can treat them all uniformly. That's the lego-block property in action: write your AMM once, and it works for every ERC20 ever deployed.

But "swap one token for another" is a deceptively hard problem when there's no central matchmaker — so before we get to how Uniswap solves it, it's worth seeing how the rest of finance does.

### The way exchanges have traditionally done it

Centralised exchanges and other market makers match bid and ask offers until an equilibrium is found. This is the **order-matching algorithm**, which typically follows the **price-time priority model** and operates within an **order book** system.

An order book is a list of all buy and sell orders placed in the market. The orders are organised by price and time, and they form the basis for matching:

- **Bid orders** represent *buy* orders, indicating the price a buyer is willing to pay.
- **Ask orders** represent *sell* orders, indicating the price a seller is willing to accept.

The order book is continuously updated as new orders are placed, modified, or cancelled. This is the most commonly used algorithm in stock exchanges — NYSE, NASDAQ, and every major venue worldwide.

**Price priority** comes first:

- Orders are matched first by price.
- Buy orders (bids) with the *highest* price are given priority over lower-priced buy orders.
- Sell orders (asks) with the *lowest* price are given priority over higher-priced sell orders.

**Time priority** breaks ties:

- If multiple orders exist at the same price, the order submitted earliest is executed first.

Concretely, suppose the order book looks like this:

> **Given** the order book is:
>
> Bids (buy orders):
> - $101 (09:00:01) — 100 shares
> - $100 (09:00:02) — 200 shares
>
> Asks (sell orders):
> - $102 (09:00:01) — 150 shares
> - $103 (09:00:02) — 100 shares
>
> **When** an incoming buy order of 50 shares at $102 is placed,
>
> **Then** the order book checks the best ask (lowest sell price), which is $102 for 150 shares,
> **And** the buy order is matched at $102, with 50 shares traded,
> **And** the remaining 100 shares at $102 stay in the order book.

### Why Uniswap doesn't do this

Solidity can do loops, and an order-matching algorithm needs them — you have to iterate through the book matching against the incoming order. But the EVM imposes a **gas limit** per transaction precisely to stop unbounded loops, and a typical order book would blow through it almost immediately. Running a serious matching engine on-chain is not viable.

So Uniswap solved the problem the other way: replace the *combinatorial* search of an order book with a *closed-form* function the EVM can evaluate in constant time.

### The Constant Product Function

Uniswap uses what's called the **Constant Product Function** to find equilibrium between buyers and sellers automatically:

> *x* × *y* = *k*

Where:

- **x** is the quantity of asset A in the liquidity pool.
- **y** is the quantity of asset B in the liquidity pool.
- **k** is a constant, representing the pool's total liquidity. *It is invariant during trades.*

**Liquidity providers** (LPs) are users who deposit capital into the pool. In return, they earn a share of every trading fee paid through it.

When a trader swaps asset A for asset B, the quantities in the pool change. If someone buys asset B out of the pool, they have to deposit a certain amount of asset A *into* the pool — exactly the amount that keeps `x * y` equal to the same `k`.

The **price** of an asset in the pool is simply the ratio of the two reserves:

- The price of A in terms of B is `y / x`.
- The price of B in terms of A is `x / y`.

### A worked example

Let's run a trade through.

> **Given** a pool of 100 ETH and 20,000 USDC,
>
> Then `x = 100`, `y = 20,000`, and `k = x * y = 2,000,000`.
> The current price of ETH is `y / x = 200 USDC per ETH`.

Now a trader wants to buy 10 ETH from the pool. After the trade, the pool will have `x = 90` ETH. For `k` to stay at 2,000,000, the USDC side must satisfy:

> `90 × y_new = 2,000,000`
> `y_new = 22,222.22 USDC`

So the trader has to deposit `22,222.22 − 20,000 = 2,222.22 USDC` to take 10 ETH out. Their effective price was `2,222.22 / 10 = 222.22 USDC per ETH` — *not* the 200 USDC that the pool was quoting before the trade.

That difference between the quoted price (200) and the realised price (222.22) is **slippage**, and it's not a flaw of the model — it's the model. The bigger the trade relative to the pool, the worse it gets. Try the same math with a 50 ETH trade and the slippage becomes dramatic.

### A small glossary

Three terms worth pinning to memory:

- **AMM** — Automated Market Maker. A swap protocol that uses a pricing curve rather than an order book.
- **Bonding curve** — the general family of curves of which `x * y = k` is one instance. Different shapes (constant sum, constant mean, stable curves) suit different asset pairs.
- **LP** — Liquidity Provider. The party who deposits the two assets into a pool and earns trading fees in return. LPs also bear **impermanent loss** when prices move — a topic for another page.
