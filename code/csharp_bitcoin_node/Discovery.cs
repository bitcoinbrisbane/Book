using System.Net;
using System.Net.Sockets;

namespace BookBitcoinNode;

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
