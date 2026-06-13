const axios = require("axios");

const encrypt = async (text) => {
  // fetch public key from keys.openpgp.org
  const response = await axios.get(
    "https://keys.openpgp.org/vks/v1/by-fingerprint/0x4F25E3B6A3D3A3D3"
  );

  if (response.status !== 200) {
    throw new Error("Failed to fetch public key");
  }

  // recipient's public key
  const publicKey = response.data;

  // encrypt message
  const password = process.env.PGP_KEY_PASSWORD || `password`;
  const privateKeyArmored = fs.readFileSync("private.key", "utf8");

  // your private key
  const privateKey = await openpgp.decryptKey({
    privateKey: await openpgp.readPrivateKey({ armoredKey: privateKeyArmored }),
    password,
  });

  const encrypted = await openpgp.encrypt({
    message: await openpgp.createMessage({ text }),
    encryptionKeys: publicKey,
    signingKeys: privateKey,
  });

  return encrypted;
};

encrypt(
  "Hello, this is a test message. Please encrypt it with your public key"
).then(console.log);
