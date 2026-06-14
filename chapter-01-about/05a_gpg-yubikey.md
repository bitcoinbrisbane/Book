# GPG on a YubiKey

The previous section generated a GPG key directly on disk. That key is a liability the moment it exists. Anyone with access to that file — a memory dump, a stolen laptop with a weak passphrase, a backup that synced to the cloud you forgot about — has your private key, forever, and you will probably never know it happened. A private key that lives on a general-purpose computer is a private key that is already compromised; you just haven't found out yet.

So don't keep one there. Move the secret material onto a hardware token like a YubiKey, where it is generated and used inside a chip that physically cannot export it. This is not a "nice to have" or an advanced topic for later. If you are going to sign commits, authenticate over SSH, or hold an encryption key that matters, do it on hardware from day one. Software keys are training wheels, and they are the kind of training wheels that get you killed.

## Why a YubiKey?

A YubiKey is a small USB device that implements the OpenPGP smart card standard. The private key lives in a tamper-resistant secure element. The host computer sends data to the card; the card performs the cryptographic operation internally and returns the result. The host never sees the key — not when it's plugged in, not when it's signing, not ever.

That single property changes the threat model entirely:

- A fully compromised laptop — rootkit, keylogger, the works — still cannot exfiltrate your signing key. The best it can do is ask the card to sign something while you're not looking, which is exactly what the touch policy below exists to stop.
- Signing or decrypting requires the physical key to be present. Your secret is now a thing you can put in your pocket, not a file that can be copied a thousand times in a millisecond.
- One device signs git commits, authenticates SSH, and decrypts mail — without that key material ever touching a disk.

Yes, there's a cost: lose the key with no backup and it's gone. People treat this as the reason to stay on software keys. It is not. It's the reason to set up recovery properly — which, done right, also means your laptop never holds a usable key. That's the whole setup, and it's the only one this book recommends. Read on.

## Prerequisites

You'll need:

- A YubiKey 5 series (4 series also works, but 5 supports Curve 25519).
- `gnupg` (already installed if you followed the previous section).
- `ykman` — the YubiKey Manager CLI, useful for inspecting and configuring the device.

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

## The Right Way: Offline Master, Subkeys on the Card

There are two ways to get keys onto a YubiKey. This is the one to use. I'll cover the other one afterwards only so you can recognise it and walk away.

You generate a full keypair on an air-gapped machine, keep the master (certify) key offline on encrypted backup media, and move only the day-to-day subkeys to the YubiKey. The master key — the one with the authority to certify identity and revoke subkeys — is never on a networked computer at all. The card holds working subkeys; your backup media holds the master. Lose the YubiKey and you haven't lost anything that matters: the offline master issues a revocation and provisions a replacement card by lunchtime.

This is the setup that delivers on the whole promise of hardware keys. Your laptop holds **no usable private key material, ever** — not the master, not the subkeys. It holds *stubs*: pointers that tell GPG "to use this key, talk to the smart card." A stolen laptop is a stolen laptop, not a stolen identity.

The high-level steps:

1. Boot a clean machine (Tails or a freshly-installed Linux USB is ideal).
2. Generate a master certify-only key plus signing, encryption, and authentication subkeys.
3. Export the master key and revocation certificate to encrypted backup media — two copies, two physical locations. Do this now, while the master exists. There is no second chance.
4. Move the subkeys to the YubiKey using `keytocard` in `gpg --edit-key`.
5. Delete the master key from the working machine, leaving only stubs that point at the YubiKey.

The full step-by-step is long enough to deserve its own write-up, and [drduh/YubiKey-Guide](https://github.com/drduh/YubiKey-Guide) already is that write-up — it's the canonical reference and the one I followed when setting mine up. Follow it end to end. Don't improvise the parts you don't understand yet.

Confirm the result:

```bash
gpg --list-secret-keys
```

Each subkey shows `ssb>` — the `>` means "the secret is on a smart card." That `>` is the entire point of this exercise. If you don't see it, the private key is still on your disk and you are not done.

## The Trap: Generating Keys On-Card

You will find guides that tell you to generate the keys directly on the YubiKey from the `gpg/card>` prompt with `admin` then `generate`. It sounds *more* secure — the key material never exists outside the secure element, not even for a moment. It is the seductive option, and for most people it is the wrong one.

Here's what those guides bury: **there is no backup, and there cannot be.** The signing and authentication subkeys can never leave the card. If the card dies, is lost, or you fat-finger the PIN three times and brick it, those keys are gone. For a signing key that's an annoyance — you rotate and move on. For the **encryption** key it is a catastrophe: every message anyone ever encrypted to that key is now permanently, irrecoverably unreadable. You don't get to apologise your way out of lost ciphertext.

On-card generation only makes sense in one narrow case: a signing-and-auth-only key you are genuinely happy to throw away and rotate, with no encryption subkey in the picture at all. That is a real but rare situation. If you have to think about whether it applies to you, it doesn't — use the offline-master setup above.

If you've read all that and still have a reason to generate on-card, the mechanics are: enter `gpg --card-edit`, then `admin`, then `generate`, and answer `n` when it offers an off-card backup of the encryption key (answer `y` only if you want an offline backup of the encryption subkey — at which point you've reinvented half of Approach 1, badly). It generates three subkeys (**[S]** Signing, **[E]** Encryption, **[A]** Authentication) at roughly 30–60 seconds each on a YubiKey 5.

## Signing With the YubiKey

Once the keys are on the card, signing works exactly as before — but GPG will prompt for the User PIN, and on a YubiKey 5 with touch policy enabled, the key will blink waiting for you to tap it.

```bash
echo "hello world" | gpg --clearsign
```

First time per session: PIN prompt. Every signing operation after that: a touch on the YubiKey if touch policy is on.

Turn touch on. All three slots. This is not optional and it is not a preference — it is the difference between a hardware key and a slow software key. Without it, malware doesn't need to steal your key; it just waits until the card is plugged in (which is most of the time) and asks it to sign whatever it likes, silently. The touch requirement means no signature happens without a human physically tapping the device. That's the property you bought the YubiKey for. Enable it and don't think about it again:

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

Use the authentication subkey as your SSH key and delete the `~/.ssh/id_*` files you've been carrying around. A software SSH key on disk has exactly the problem this whole chapter is about — it's a copyable secret sitting on a general-purpose computer. The auth subkey you already have on the card does the same job and can't be stolen off the machine. One key, one device to protect, one thing to lose. Don't keep a second.

Enable the GPG agent's SSH support:

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
- **Lost YubiKey** — publish a revocation with your offline revocation certificate, then provision a replacement from your offline master backup. Total damage: one afternoon and the price of a new key. If your reaction to this scenario is "wait, I don't have an offline master" then you took the trap, and a lost key now means a lost identity. This is the entire reason the offline-master setup is the only one this book recommends.

## References

- [1] Yubico. *OpenPGP Smart Card Specification* — yubico.com/products/yubikey-5-overview
- [2] drduh. *YubiKey-Guide* — github.com/drduh/YubiKey-Guide
- [3] GnuPG. *Using the OpenPGP card with GnuPG* — gnupg.org/howtos/card-howto/en
- [4] Yubico. *YubiKey Manager (ykman) documentation*

See [Bibliography](../bibliography.md) for full citations.
