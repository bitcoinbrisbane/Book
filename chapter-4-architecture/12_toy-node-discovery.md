## Toy node — Discovery

Discovery is the first thing a fresh node does: *who do I even talk to?* Bitcoin's answer, which we mirror in our toy, is a hardcoded list of DNS seed hostnames. Each one resolves to dozens of recently-active peers, kept fresh by volunteers running open-source crawlers. Hardcoding eight hostnames is enough to bootstrap an entire decentralised network — the IPs change, the names don't.

## The seed list

```csharp
public static readonly string[] DnsSeeds =
{
    "seed.bitcoin.sipa.be",
    "dnsseed.bluematt.me",
    "seed.bitcoinstats.com",
    "seed.bitcoin.jonasschnelli.ch",
    "seed.btc.petertodd.org",
    "seed.bitcoin.sprovoost.nl",
    "dnsseed.emzy.de",
    "seed.bitcoin.wiz.biz",
};
```

These are the same hostnames hardcoded in [Bitcoin Core's `chainparams.cpp`](https://github.com/bitcoin/bitcoin/blob/master/src/chainparams.cpp). The policy each operator must follow lives at [`doc/dnsseed-policy.md`](https://github.com/bitcoin/bitcoin/blob/master/doc/dnsseed-policy.md). They run custom `BIND` zones that respond with rotating sets of recently-seen full nodes.

If every one of these hostnames went down at the same moment, new nodes couldn't bootstrap — which is why Bitcoin Core *also* ships a list of hardcoded fallback IPs and supports `-addnode` and `-connect` flags. Our toy node has none of that; if DNS fails, we fail.

## The resolver

```csharp
public static class Discovery
{
    public static async Task<List<IPEndPoint>> ResolveSeedsAsync(CancellationToken ct)
    {
        var endpoints = new List<IPEndPoint>();
        foreach (var seed in NetworkConstants.DnsSeeds)
        {
            try
            {
                var addrs = await Dns.GetHostAddressesAsync(seed, ct);
                var v4 = addrs.Where(a => a.AddressFamily == AddressFamily.InterNetwork).ToArray();
                foreach (var a in v4)
                    endpoints.Add(new IPEndPoint(a, NetworkConstants.MainnetPort));
                Console.WriteLine($"    {seed,-35} {v4.Length} addresses");
            }
            catch (Exception ex)
            {
                Console.WriteLine($"    {seed,-35} {ex.Message}");
            }
        }
        return endpoints;
    }
}
```

There's almost nothing to it. We hand each hostname to `System.Net.Dns.GetHostAddressesAsync`, filter to IPv4 (purely to keep the connection logic short), and pair each address with port 8333 — Bitcoin's well-known port. A successful run fans out to roughly 150–200 candidates from those eight hostnames.

The output looks like:

```
    seed.bitcoin.sipa.be                25 addresses
    dnsseed.bluematt.me                 21 addresses
    seed.bitcoinstats.com               24 addresses
    …
```

That's our peer pool. The `PeerSession` class (covered next page) consumes this list lazily — it walks the queue trying to handshake, and any reconnect during IBD just dequeues the next candidate.

## What a real node does that we don't

- **`addr` / `addrv2` gossip.** Once handshaked, peers exchange known peer addresses with each other. A node that's been online for a while no longer needs the DNS seeds — it has a populated peer database it persists across restarts (`peers.dat` in Bitcoin Core).
- **Reachability scoring.** Bitcoin Core scores peers on uptime, response latency and protocol compliance, and prefers high-scoring peers on reconnect.
- **Tor/I2P support.** `addrv2` exists specifically to carry onion and I2P addresses; we filter to IPv4 only.
- **Filtering by services flag.** The DNS seed responds with recent peers but doesn't tell you which ones are pruned, archival, witness-serving, etc. Real nodes filter by the `services` field in the `version` message.

For the pedagogical goal — *show how a fresh node finds its first peer* — eight DNS lookups is a complete and accurate answer.
