---
layout: post
title: "Port crypto-js AES functions to C#"
description: "Okay, I'm porting some modules from Nodejs to C# and I couldn't find any built-in modules or libraries to do this so I had to implement it manually, luckily, with the help from Stackoverflow."
categories: [.net]
tags: []
thumbnail:
---

Okay, I'm porting some modules from Nodejs to C# and I couldn't find any
built-in modules or libraries to do this so I had to implement it manually, luckily, with the help
from Stackoverflow.

I have a message that was encrypted using `crypto-js` and stored in the database. Here is the
Nodejs code that generates the encrypted data

```javascript
const cryptojs = require('crypto-js');
const encryptedMsg = cryptojs.AES.encrypt('message', 'secret').toString();
```

The result is a string that looks like this

```
U2FsdGVkX184KJolbrZkg8w+rX/V9OW7sbUvWPVogdY=
```

Now, I need to read it back in C# and decrypt it to get the original message.
The built-in `Aes` class in C# requires a Key and an IV to be explicitly passed in but
there is no utility to generate the Key and the IV from a specified string. The above `encrypt`
method from **crypto-js** is a simplified and implicit version of the Key and the IV. It doesn't play
well with C# and actually is not the AES standard (**crypto-js** still allows you to pass
in the Key and IV explicitly).

> For AES Cipher Algorithm, we need a Key and an IV (Initialization Vector) to add
> randomness to the encrypted data.

After playing around with `crypto-js` code base and with the help from
[Stackoverflow](https://stackoverflow.com/questions/35472396/how-does-cryptojs-get-an-iv-when-none-is-specified),
I finally figured out how the data is stored and how the Key/IV are generated.
In order to derive a key from the passphrase, it uses the OpenSSL-compatible derivation function
[EVP_BytesToKey](https://www.openssl.org/docs/manmaster/man3/EVP_BytesToKey.html). Here are the
steps

- Generate a random 8byte **salt**.
- Use it along with the input passphrase to generate the Key and the IV.
- The Key and the IV are then fed into AES function to produce the ciphertext.
- The final result is a base64-encoded string containing the `Salted__` string at the beginning
  followed by the 8byte **salt** and the actual ciphertext.

<!-- more -->

> Simply base64-decode the above string to verify, we get this result `Salted__8(�%n�d��>���廱�/X�h��`

However, I couldn't find the equivalent function of `EVP_BytesToKey` in C#. And yeah,
[Stackoverflow](https://stackoverflow.com/questions/8008253/c-sharp-version-of-openssl-evp-bytestokey-method)
came to the rescue again. This method produces a 32byte Key and a 16byte IV, using `MD5`, just
like the default behavior of `crypto-js`.

```csharp
void DeriveKeyAndIv(byte[] passphrase, byte[] salt, int iterations, out byte[] key, out byte[] iv)
{
    var hashList = new List<byte>();

    var preHashLength = passphrase.Length + (salt?.Length ?? 0);
    var preHash = new byte[preHashLength];

    Buffer.BlockCopy(passphrase, 0, preHash, 0, passphrase.Length);
    if (salt != null)
        Buffer.BlockCopy(salt, 0, preHash, passphrase.Length, salt.Length);

    var hash = MD5.Create();
    var currentHash = hash.ComputeHash(preHash);

    for (var i = 1; i < iterations; i++)
    {
        currentHash = hash.ComputeHash(currentHash);
    }

    hashList.AddRange(currentHash);

    while (hashList.Count < 48) // for 32-byte key and 16-byte iv
    {
        preHashLength = currentHash.Length + passphrase.Length + (salt?.Length ?? 0);
        preHash = new byte[preHashLength];

        Buffer.BlockCopy(currentHash, 0, preHash, 0, currentHash.Length);
        Buffer.BlockCopy(passphrase, 0, preHash, currentHash.Length, passphrase.Length);
        if (salt != null)
            Buffer.BlockCopy(salt, 0, preHash, currentHash.Length + passphrase.Length, salt.Length);

        currentHash = hash.ComputeHash(preHash);

        for (var i = 1; i < iterations; i++)
        {
            currentHash = hash.ComputeHash(currentHash);
        }

        hashList.AddRange(currentHash);
    }

    hash.Clear();
    key = new byte[32];
    iv = new byte[16];
    hashList.CopyTo(0, key, 0, 32);
    hashList.CopyTo(32, iv, 0, 16);
}
```

The remaining job is to extract the **salt** from the encrypted string by reading the bytes, use that to
calculate the Key and the IV to input to AES
(read more at [Aes Class](https://docs.microsoft.com/en-us/dotnet/api/system.security.cryptography.aes?view=net-5.0)).

```csharp
public static string DecryptAes(string encryptedString, string passphrase)
{
    // encryptedString is a base64-encoded string starting with "Salted__" followed by a 8-byte salt and the
    // actual ciphertext. Split them here to get the salted and the ciphertext
    var base64Bytes = Convert.FromBase64String(encryptedString);
    var saltBytes = base64Bytes[8..16];
    var cipherTextBytes = base64Bytes[16..];

    // get the byte array of the passphrase
    var passphraseBytes = Encoding.UTF8.GetBytes(passphrase);

    // derive the key and the iv from the passphrase and the salt, using 1 iteration
    // (cryptojs uses 1 iteration by default)
    DeriveKeyAndIv(passphraseBytes, saltBytes, 1, out var keyBytes, out var ivBytes);

    // create the AES decryptor
    using var aes = Aes.Create();
    aes.Key = keyBytes;
    aes.IV = ivBytes;
    // here are the config that cryptojs uses by default
    // https://cryptojs.gitbook.io/docs/#ciphers
    aes.KeySize = 256;
    aes.Padding = PaddingMode.PKCS7;
    aes.Mode = CipherMode.CBC;
    var decryptor = aes.CreateDecryptor(keyBytes, ivBytes);

    // example code on MSDN https://docs.microsoft.com/en-us/dotnet/api/system.security.cryptography.aes?view=net-5.0
    using var msDecrypt = new MemoryStream(cipherTextBytes);
    using var csDecrypt = new CryptoStream(msDecrypt, decryptor, CryptoStreamMode.Read);
    using var srDecrypt = new StreamReader(csDecrypt);

    // read the decrypted bytes from the decrypting stream and place them in a string.
    return srDecrypt.ReadToEnd();
}
```

See the full working file
[here](/files/2021-08-14-port-crypto-js-to-csharp/CryptoJsStaticHelper.cs).

**Final words**

- The above solution contains many little hacks, I don't feel good about it, but at least, it works.
- The function `EVP_BytesToKey` is not secure since it uses MD5 hashing function, which is very fast
  and **crypto-js** also uses only 1 iteration to perform the hash.
- My bad (several years ago) for not researching AES when using it! Next time, I should read
  carefully about what I plan to apply, don't just choose the easy and fast solution. 😂
