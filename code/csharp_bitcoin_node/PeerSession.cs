using System.Net;
using System.Net.Sockets;

namespace BookBitcoinNode;

// Owns the currently-active peer connection and a queue of fallbacks.
// When a peer drops, callers Drop() and EnsureConnected() to roll over
// to the next candidate without losing IBD progress.
public sealed class PeerSession : IAsyncDisposable
{
    private readonly Queue<IPEndPoint> _candidates;
    private readonly CancellationToken _ct;
    private PeerConnection? _peer;
    private IPEndPoint? _endpoint;

    public PeerSession(IEnumerable<IPEndPoint> candidates, CancellationToken ct)
    {
        _candidates = new Queue<IPEndPoint>(candidates);
        _ct = ct;
    }

    public PeerConnection Peer =>
        _peer ?? throw new InvalidOperationException("not connected");

    public IPEndPoint? Endpoint => _endpoint;

    public async Task<bool> EnsureConnectedAsync(int startHeight)
    {
        if (_peer != null) return true;
        while (_candidates.Count > 0)
        {
            var ep = _candidates.Dequeue();
            try
            {
                var p = new PeerConnection();
                using var timeout = CancellationTokenSource.CreateLinkedTokenSource(_ct);
                timeout.CancelAfter(TimeSpan.FromSeconds(10));
                await p.ConnectAsync(ep, timeout.Token);
                await p.HandshakeAsync(startHeight, timeout.Token);
                _peer = p;
                _endpoint = ep;
                Console.WriteLine($"    connected: {ep}");
                return true;
            }
            catch (Exception ex)
            {
                Console.WriteLine($"    {ep}: {ex.Message}");
            }
        }
        return false;
    }

    public async Task DropAsync()
    {
        if (_peer != null) await _peer.DisposeAsync();
        _peer = null;
        _endpoint = null;
    }

    public ValueTask DisposeAsync() => new(DropAsync());

    public static bool IsPeerDisconnect(Exception ex) =>
        ex is EndOfStreamException
        or IOException
        or SocketException
        or InvalidDataException
        or OperationCanceledException;
}
