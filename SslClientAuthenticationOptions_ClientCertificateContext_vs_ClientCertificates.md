# Question and Answer: `SslClientAuthenticationOptions.ClientCertificateContext` vs `ClientCertificates`

**User question:**

> When would you use the `SslClientAuthenticationOptions.ClientCertificateContext` property as opposed to just the `ClientCertificates` property as in your code example?

---

**Answer:**

Excellent question — that property (`SslClientAuthenticationOptions.ClientCertificateContext`) is one of the newer, more advanced knobs introduced in .NET 8 and 9, and it’s easy to miss *why* it exists if you’ve only ever used `ClientCertificates`.

Let’s break it down clearly:

---

## 🔹 `ClientCertificates` (the classic way)

`ClientCertificates` is simply a collection of `X509Certificate` objects.  
When you call `AuthenticateAsClientAsync`, the TLS stack will:

1. Look at what certificates are in the collection.  
2. Choose one (if the server requests a client cert).  
3. Use the private key from that `X509Certificate2` object to sign the handshake.

That’s fine for most applications — you just load a `.pfx`, add it to the collection, and you’re done.  
However, it assumes the certificate and private key are both in memory and available via `X509Certificate2`.  

---

## 🔹 `ClientCertificateContext` (the modern, performance-oriented / custom-key way)

`ClientCertificateContext` wraps the same information, but in a lower-level, *pre-bound* form that .NET can reuse across many connections efficiently. It’s used when you need:

### 1. **Performance / connection reuse**

When you create a `SslClientCertificateContext` (via `SslClientCertificateContext.Create()`), it binds together:

- The client certificate chain,
- The private key handle (possibly hardware or OS key),
- Cryptographic settings used in the handshake.

If you open *many* TLS connections (e.g., in a connection pool), using a shared `ClientCertificateContext` avoids repeatedly parsing the `.pfx` and re-building the chain for every connection.

**Example use:**

```csharp
var cert = new X509Certificate2("client.pfx", "secret",
    X509KeyStorageFlags.EphemeralKeySet);

var context = SslClientCertificateContext.Create(cert);

var sslOptions = new SslClientAuthenticationOptions
{
    TargetHost = "example.com",
    ClientCertificateContext = context
};
```

This can reduce CPU and memory churn for high-throughput or short-lived clients.

---

### 2. **Hardware-bound or custom key scenarios**

If your private key isn’t a normal software key — for example, it lives:

- On a hardware security module (HSM),
- In a TPM or smart card,
- Managed via a custom `AsymmetricAlgorithm` or KeyVault provider,

then `ClientCertificateContext` lets you bind that non-exportable key handle explicitly to the TLS context.  
This can’t be done cleanly using `ClientCertificates` (which assumes a normal `X509Certificate2` with an accessible key).

---

### 3. **Explicit control over chain and trust building**

`ClientCertificateContext.Create()` has overloads that accept the full certificate chain and configuration for how to build it.  
That’s useful in environments where you can’t rely on the local certificate store or want deterministic chain construction.

---

## 🔹 How to choose between them

| Use case | Recommended property |
|-----------|----------------------|
| Simple mTLS with one client cert | `ClientCertificates` |
| Short-lived test client | `ClientCertificates` |
| High-performance client with many parallel TLS connections | `ClientCertificateContext` |
| Certificate from HSM/TPM/KeyVault | `ClientCertificateContext` |
| Need to pre-build or pin full chain | `ClientCertificateContext` |

---

### TL;DR

- **`ClientCertificates`**: simple, automatic, good for most apps.  
- **`ClientCertificateContext`**: manual but efficient; use for large-scale, performance-sensitive, or hardware-keyed clients.

---
