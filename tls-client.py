import logging
import pika
import ssl
import time

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

logger.info('ssl.HAS_SNI: %s', ssl.HAS_SNI)

context = ssl.create_default_context(cafile="./tls-gen/basic/result/ca_certificate.pem")

context.load_cert_chain("./tls-gen/basic/result/client_certificate.pem",
                        "./tls-gen/basic/result/client_key.pem")

ssl_options = pika.SSLOptions(context, server_hostname="shostakovich")

conn_params = pika.ConnectionParameters(port=5671, ssl_options=ssl_options)
with pika.BlockingConnection(conn_params) as conn:
    ch = conn.channel()
    ch.queue_declare("foobar")
    ch.basic_publish("", "foobar", "Hello, world!")
    logger.info(ch.basic_get("foobar"))
    logger.info('sleeping 600 seconds...')
    time.sleep(600)
