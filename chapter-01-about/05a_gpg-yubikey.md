# GPG on a YubiKey

The previous section generated a GPG key directly on disk. Anyone with access to that file — or a memory dump, or a stolen laptop with a weak passphrase — has your private key. Moving the private key onto a hardware token like a YubiKey solves this: the secret material is generated on the device, never leaves it, and every signing or decryption operation requires the physical key to be present and (optionally) touched.

## Why a YubiKey?

A YubiKey is a small USB device that implements the OpenPGP smart card standard. The private key lives in a tamper-resistant secure element. The host computer sends data to the card; the card performs the cryptographic operation internally and returns the result. The host never sees the key.

Practical implications:

- A compromised laptop cannot exfiltrate your signing key.
- Signing or decrypting requires the YubiKey to be plugged in — and on YubiKey 5 series, optionally a physical touch per operation.
- The same key can sign git commits, authenticate over SSH, and decrypt PGP email without ever being copied.

The trade-off is recovery: if you lose the YubiKey and have no backup, the key is gone. We address that below.

## Prerequisites

You'll need:

- A YubiKey 5 series (4 series also works, but 5 supports Curve 25519).
- `gnupg` (already installed if you followed the previous section).
- `ykman` — the YubiKey Manager CLI, useful for inspecting and configuring the device.

On macOS:

```bash
brew install gnupg ykman pinentry-mac
```

On Debian/Ubuntu:

```bash
sudo apt install gnupg2 yubikey-manager scdaemon pcscd
```

Plug in the YubiKey and confirm GPG can see it:

```bash
gpg --card-status
```

You should see output starting with:

```text
Reader ...........: Yubico YubiKey ...
Application ID ...: D2760001240103040006XXXXXXXX0000
Version ..........: 3.4
Manufacturer .....: Yubico
```

## Changing the Default PINs

A factory-reset YubiKey ships with well-known default PINs:

- **User PIN**: `123456` — required for signing, decryption, and authentication.
- **Admin PIN**: `12345678` — required for management operations like loading keys or changing PINs.

Change both before doing anything else. Enter the card admin shell:

```bash
gpg --card-edit
```

At the `gpg/card>` prompt:

```text
admin
passwd
```

Select option `1` to change the User PIN, then `3` to change the Admin PIN. Pick strong PINs — the YubiKey will block after 3 wrong User PIN attempts and 3 wrong Admin PIN attempts, after which the card must be reset (destroying any keys on it).

## Approach 1: Generate Keys On-Card

The most secure approach is to generate the keys directly on the YubiKey. The private key material never exists outside the secure element.

The downside: there is no backup. If the YubiKey is lost or destroyed, the key is unrecoverable. For a signing key this is usually fine — you just rotate. For an encryption key it means every message encrypted to that key is permanently unreadable.

From the `gpg/card>` prompt:

```text
admin
generate
```

GPG will ask whether to make an off-card backup of the encryption key — answer `n` for the strictest model, `y` if you want a paper or offline backup of the encryption subkey only (the signing and authentication subkeys cannot be backed up this way).

You'll be prompted for the Admin PIN, key expiry, name, and email. The card will generate three subkeys:

- **[S]** Signing
- **[E]** Encryption
- **[A]** Authentication

This takes around 30–60 seconds per subkey on a YubiKey 5.

## Approach 2: Generate Off-Card, Then Move Subkeys

This is the more common workflow for power users. You generate a full keypair on an air-gapped machine, keep the master (certify) key offline on encrypted backup media, and move only the day-to-day subkeys to the YubiKey. If the YubiKey is lost, the offline master can issue revocations and provision a replacement.

The high-level steps:

1. Boot a clean machine (Tails or a freshly-installed Linux USB is ideal).
2. Generate a master certify-only key plus signing, encryption, and authentication subkeys.
3. Export the master key and revocation certificate to encrypted backup media (two copies, stored in different physical locations).
4. Move the subkeys to the YubiKey using `keytocard` in `gpg --edit-key`.
5. Delete the master key from the working machine, leaving only stubs that point at the YubiKey.

The detailed walkthrough is long enough to deserve its own write-up — [drduh/YubiKey-Guide](https://github.com/drduh/YubiKey-Guide) is the canonical reference and the one I followed when setting mine up. The important conceptual point is: after this process, your laptop holds no private key material at all. It holds *stubs* — pointers that tell GPG "to use this key, talk to the smart card."

You can confirm this with:

```bash
gpg --list-secret-keys
```

The output will show `ssb>` (with a `>` suffix) rather than plain `ssb` for each subkey — the `>` indicates the secret is on a smart card.

## Signing With the YubiKey

Once the keys are on the card, signing works exactly as before — but GPG will prompt for the User PIN, and on a YubiKey 5 with touch policy enabled, the key will blink waiting for you to tap it.

```bash
echo "hello world" | gpg --clearsign
```

First time per session: PIN prompt. Every signing operation after that: a touch on the YubiKey if touch policy is on.

To enable touch-to-sign (recommended — it prevents malware from silently signing while the card is plugged in):

```bash
ykman openpgp keys set-touch sig on
ykman openpgp keys set-touch enc on
ykman openpgp keys set-touch aut on
```

## Signing Git Commits

Configure git to use the signing subkey on the YubiKey. First, find the signing subkey ID:

```bash
gpg --list-secret-keys --keyid-format=long
```

Look for the subkey line ending in `[S]`. Then:

```bash
git config --global user.signingkey <SIGNING_SUBKEY_ID>!
git config --global commit.gpgsign true
```

The trailing `!` tells GPG to use that specific subkey rather than picking one itself. Every `git commit` now requires the YubiKey to be present and (with touch enabled) tapped.

## Using the YubiKey for SSH

The authentication subkey can replace your SSH key entirely. Enable the GPG agent's SSH support:

```bash
echo "enable-ssh-support" >> ~/.gnupg/gpg-agent.conf
gpgconf --kill gpg-agent
```

Then in your shell rc file:

```bash
export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
```

Export the SSH public key derived from your authentication subkey:

```bash
ssh-add -L
```

That's the line you paste into `~/.ssh/authorized_keys` on remote hosts, or into GitHub's SSH key settings. From now on, SSH authentication requires the YubiKey.

## Moving to a New Machine

The setup is portable. On a new laptop:

1. Import your public key: `gpg --import public.asc` (or pull from a key server).
2. Mark it trusted: `gpg --edit-key <KEY> trust` and select `5` (ultimate).
3. Plug in the YubiKey and run `gpg --card-status`.

GPG will detect the card and automatically create the stub entries pointing at it. No private key material is ever copied.

## When Things Go Wrong

A few failure modes worth knowing:

- **`gpg: signing failed: No such device`** — the agent has stale state. Run `gpgconf --kill gpg-agent` and retry.
- **Card not detected on Linux** — `pcscd` isn't running, or `scdaemon` is fighting with another smart card service. `systemctl restart pcscd` usually fixes it.
- **`PIN blocked`** — you entered the wrong User PIN 3 times. Unblock with the Admin PIN via `gpg --card-edit` → `admin` → `passwd` → option `2` (unblock PIN).
- **Lost YubiKey** — use your offline revocation certificate to publish a revocation, then provision a replacement from your offline master key backup. This is why Approach 2 is worth the extra setup.

## References

- [1] Yubico. *OpenPGP Smart Card Specification* — yubico.com/products/yubikey-5-overview
- [2] drduh. *YubiKey-Guide* — github.com/drduh/YubiKey-Guide
- [3] GnuPG. *Using the OpenPGP card with GnuPG* — gnupg.org/howtos/card-howto/en
- [4] Yubico. *YubiKey Manager (ykman) documentation*

See [Bibliography](../bibliography.md) for full citations.
