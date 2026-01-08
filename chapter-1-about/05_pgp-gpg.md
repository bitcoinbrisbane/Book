# PGP/GPG

Pretty Good Privacy (PGP) and its open-source counterpart GNU Privacy Guard (GPG) are cryptographic software used for encrypting and decrypting data, as well as for digital signing and verifying messages or files. These tools provide a high level of security and privacy by utilising asymmetric encryption, enabling users to communicate securely over insecure channels like the internet. Here's an overview of their history and significance:

## History

PGP was developed by Phil Zimmermann in 1991 as a response to concerns about government surveillance and the need for privacy in digital communications. Zimmermann released PGP as freeware, making strong encryption accessible to the public. This resulted in widespread adoption and recognition for its robust encryption capabilities.

GNU Privacy Guard (GPG) is an open-source implementation of the OpenPGP standard and serves as a free alternative to PGP. It was developed by Werner Koch and released in 1999, offering similar functionalities as PGP but as free software under the GNU General Public License (GPL).

## How It Works

PGP and GPG are primarily used for secure communication and data integrity. They employ asymmetric encryption, where users have a public key for encrypting messages/files that only the intended recipient, who possesses the corresponding private key, can decrypt. This ensures confidentiality in communications.

Digital signing is another vital aspect; users can sign their messages/files with their private key, allowing recipients to verify the authenticity of the sender using the sender's public key. This provides assurance of the message's origin and integrity.

These tools are crucial for protecting sensitive information, such as personal communications, financial data, intellectual property, and more, particularly in situations where privacy and security are paramount, like in journalism, business communications, legal matters, and personal privacy protection.

## Adoption Challenges

That being said, PGP adoption is terrible at best. There are some great email clients, such as Canary and eM Client, as well as plugins for Gmail.

All that being said, it's still very hard to get adoption. I've dealt with some security firms who still struggle, and Facebook/Meta just quietly announced they would drop support for notifications to be encrypted using PGP. Wired magazine announced it was "dead" in circa 2018 for those reasons - it's just too hard to get users onboard.

It's a shame, as it would also help solve email phishing attacks.

## Creating a GPG Key

To create our GPG key in terminal:

```bash
gpg --full-generate-key
```

Select the following options:
- `9` - ECC (sign and encrypt)
- `1` - Curve 25519
- `0` - Key does not expire

Add a strong password and you've created the key.

## Publishing to a Key Server

Next we should push to one or more key servers. We can do this in terminal:

```bash
gpg --send-keys [KEY ID]
```

For example:

```bash
gpg --send-keys F68AA906B5BE601AC49EF83DC61F975DEBDEDC6B
```

This should display:

```
gpg: sending key C61F975DEBDEDC6B to hkps://keys.openpgp.org
```

If that was successful you should now be able to search on keys.openpgp.org by the email address, key ID or its fingerprint which was also displayed back to the terminal as `C61F975DEBDEDC6B`.

To specify the key server, you can check the documentation of each key server.

## Listing Keys

Now we can list our keys:

```bash
gpg --list-keys
```

## Importing a Recipient's Public Key

If we want to lookup a recipient's public key, we can search our key server. Note, key servers are not synced - perhaps this would be a good blockchain?

In a browser, navigate to https://keys.openpgp.org/, type the email or fingerprint and search. If the public key is found, click the hyperlink to download the `.asc` file. If you're using the default Firefox browser, it should be saved to your `~/Downloads` folder with the ID of the key as the filename.

You can import it into the OS with the following command:

```bash
gpg --import [KEY ID].asc
```

For my lucas@lucascullen.com public key, it would be:

```bash
gpg --import 93455CE0D1A7AC071201CB7E52E769C9FFA25B92.asc
```

Then run `gpg --list-keys` to double check it's imported. Now that you have done this, feel free to send me an encrypted email!  Below is some Node.js code to fetch the key from the key server and keep it in memory instead of downloading to disk, then encrypt a message.

```javascript
const openpgp = require('openpgp');
const axios = require('axios');

// Function to fetch a public key from keys.openpgp.org
async function fetchPublicKey(email) {
  const url = `https://keys.openpgp.org/vks/v1/by-email/${encodeURIComponent(email)}`;

  try {
    const response = await axios.get(url);
    return response.data;
  } catch (error) {
    throw new Error(`Failed to fetch key: ${error.message}`);
  }
}

// Example usage: Fetch and encrypt a message
async function encryptMessage(email, messageText) {
  try {
    // Fetch the public key
    const publicKeyArmored = await fetchPublicKey(email);
    console.log('Public key fetched successfully');

    // Read the public key
    const publicKey = await openpgp.readKey({ armoredKey: publicKeyArmored });

    // Create encrypted message
    const message = await openpgp.createMessage({ text: messageText });
    const encrypted = await openpgp.encrypt({
      message,
      encryptionKeys: publicKey
    });

    console.log('Encrypted message:');
    console.log(encrypted);

    return encrypted;
  } catch (error) {
    console.error('Error:', error.message);
  }
}

// Run the example
encryptMessage('lucas@lucascullen.com', 'Hello, this is a secret message!');
```

To use this code, first install the required libraries:

```bash
npm install openpgp axios
```

## Exporting Your Private Key

You will note that the Node.js code reads the private key from a file. Here's how to export it or display in terminal as plain text:

```bash
cd ~/Downloads
gpg --export-secret-keys lucas@lucascullen.com > lucas_private_key.asc
```

Now if we cat the file:

```bash
cat lucas_private_key.asc
```

You will see it's not ASCII encoded. We can "fix" this by using the `--armor` flag:

```bash
gpg --export-secret-keys --armor lucas@lucascullen.com > lucas_private_key.asc
cat lucas_private_key.asc
```

You should now see a private key block, something like:

```text
-----BEGIN PGP PRIVATE KEY BLOCK-----
lQcXBF8EGIoBEACq3GiUIDAn7bonDUJqaz2XkW1QStLxRhVxElxE8FCoLdkZ1lkQ
qozKm51B+qwIzboCP1jA7uIg4++ZXjcl3TKJ+2e9LGsIH5SomLSI/k21hqGFO4mt
-----END PGP PRIVATE KEY BLOCK-----
```

As this is ASCII, we can import into our program as a string or using a file reader.  Go ahead and send me an encrypted message!

## References

- [1] Zimmermann, P. (1991). *Why I Wrote PGP*
- [2] Koch, W. (1999). *GnuPG - The GNU Privacy Guard*
- [3] OpenPGP Key Server - keys.openpgp.org
- [4] Wired Magazine (2018). *The World's Email Encryption Software Relies on One Guy, Who is Going Broke*
- [5] Meta/Facebook (2024). Discontinuation of PGP-encrypted notification emails
- [6] Free Software Foundation. *GNU General Public License*
- [7] Canary Mail - Encrypted email client
- [8] eM Client - Email client with PGP support

See [Bibliography](../bibliography.md) for full citations.
