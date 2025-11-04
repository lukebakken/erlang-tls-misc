namespace TlsClient
{
    using System;
    using System.IO;
    using System.Net.Security;
    using System.Net.Sockets;
    using System.Security.Authentication;
    using System.Security.Cryptography.X509Certificates;
    using System.Threading.Tasks;

    class Program
    {
        private const string Host = "localhost";
        private const int Port = 4433;

        static async Task Main()
        {
            using var tcpClient = new TcpClient();
            await tcpClient.ConnectAsync(Host, Port);

            using NetworkStream networkStream = tcpClient.GetStream();

            using var sslStream = new SslStream(
                networkStream,
                leaveInnerStreamOpen: false,
                userCertificateValidationCallback: ValidateServerCertificate);

            var clientCertificate = new X509Certificate2(
                    fileName: "../certs/client_certificate.pfx",
                    password: (string?)null,
                    keyStorageFlags: X509KeyStorageFlags.MachineKeySet | X509KeyStorageFlags.Exportable);

            var sslOptions = new SslClientAuthenticationOptions
            {
                TargetHost = Host, // must match server cert name
                EnabledSslProtocols = SslProtocols.Tls12 | SslProtocols.Tls13,
                CertificateRevocationCheckMode = X509RevocationMode.NoCheck,
                ClientCertificates = new X509CertificateCollection { clientCertificate }
            };

            await sslStream.AuthenticateAsClientAsync(sslOptions);

            Console.WriteLine("[INFO] TLS handshake succeeded.");
            Console.WriteLine($"[INFO] negotiated protocol: {sslStream.SslProtocol}");
            Console.WriteLine($"[INFO] negotiated cipher:   {sslStream.NegotiatedCipherSuite}");

            string request = $"HELLO!\r\n\r\n";
            using var writer = new StreamWriter(sslStream) { AutoFlush = true };
            await writer.WriteAsync(request);

            using var reader = new StreamReader(sslStream);
            string? line;
            while ((line = await reader.ReadLineAsync()) != null)
            {
                Console.WriteLine(line);
            }
        }

        private static bool ValidateServerCertificate(
            object sender,
            X509Certificate? certificate,
            X509Chain? chain,
            SslPolicyErrors sslPolicyErrors)
        {
            Console.WriteLine("[INFO] yolo-accepting certificate: {0}", certificate);
            Console.WriteLine("[INFO] policy errors: {0}", sslPolicyErrors);
            return true;
        }
    }
}
