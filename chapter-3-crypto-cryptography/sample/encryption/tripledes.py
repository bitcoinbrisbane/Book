from Crypto.Cipher import DES3
from Crypto.Util.Padding import pad, unpad
from Crypto.Random import get_random_bytes
import base64

# Define the key and IV (Initialization Vector)
key = b'0123456789abcdef01234567'  # 24 bytes key for 3DES
iv = b'12345678'  # 8 bytes IV for DES

# Function to encrypt plaintext using 3DES
def encrypt(text):
    cipher = DES3.new(key, DES3.MODE_CBC, iv)
    padded_text = pad(text.encode('utf-8'), DES3.block_size)
    encrypted = cipher.encrypt(padded_text)
    return base64.b64encode(encrypted).decode('utf-8')

# Function to decrypt ciphertext using 3DES
def decrypt(encrypted_text):
    encrypted_data = base64.b64decode(encrypted_text)
    cipher = DES3.new(key, DES3.MODE_CBC, iv)
    decrypted = unpad(cipher.decrypt(encrypted_data), DES3.block_size)
    return decrypted.decode('utf-8')

# Add a timer
import time
start_time = time.time()

# Test the encryption and decryption
plaintext = 'Hello, this is a plaintext message!'
print('Plaintext:', plaintext)

encrypted_text = encrypt(plaintext)
print('Encrypted:', encrypted_text)

decrypted_text = decrypt(encrypted_text)
print('Decrypted:', decrypted_text)

# Print the time taken
print('Time taken:', time.time() - start_time)