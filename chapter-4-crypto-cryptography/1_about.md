As your probably aware, computers are determistc, given a set of inputs they should always return the same outputs.  If they didnt, it would be pretty chaotic using a computer!  Random Number Generators or RNGs are ....  The are USB hardware devices like the TrueRNG by ubld.it that create randomness for your device or server for around $100 USD. 

Computers and software however, use Psudeo Random Number Generators to create a random output.  However, they rely on a "seed" input, often the computers time stamp.   When creating passwords or perhaps more importantly crypto private keys, using a random number is critical.  There is a good YouTube video by the former hacker "King Pin" where he cracks a password on a bitcoin wallet to recover a few million dollars (at time of writing) with of bitcoin.  The software used to generate the password took the computers time as the seed to generate the password.  The attackers took a range of dates where they thought the original password was generated and replayed all the milliseconds in that range.  The attack was successful and the coins where recovered!

### RC4

```text
i = (i + 1) mod 256
j = (j + Si) mod 256
swap Si and Sj
t = (Si + Sj) mod 256
K = St
```

Ciphertext (in base64): `wqLCthTDlMKvZR7Dh8Kvf1JZw5s=`
Decrypted text: `Hello, World!`