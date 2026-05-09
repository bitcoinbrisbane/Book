namespace BookBitcoinNode;

public sealed class Mempool
{
    private readonly List<Transaction> _txs = new();

    public IReadOnlyList<Transaction> Transactions => _txs;
    public int Count => _txs.Count;

    public void Add(Transaction tx) => _txs.Add(tx);
    public void Clear() => _txs.Clear();
}
