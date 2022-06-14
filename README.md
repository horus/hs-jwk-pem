# Convert RSA JWK to PEM Format

_Disclaimer: Low quality code for own private purpose only. Use at your own risk._

## Run

As a CLI tool:

```
stack run < ./key.json
```

...or as a tiny web service:

```
stack run -- web
```

### Docker

For simplicity, one can build local docker images with the given Dockerfile.

```
docker build -t hs-jwk-pem .
docker run -it -p 8080:9000 --rm hs-jwk-pem
```

### Web API

Example request:

```
curl \
  --request POST \
  --url http://127.0.0.1:9000/ \
  --header 'Content-Type: application/json' \
  --data '{"kty": "RSA","n": "...","e": "AQAB"}'
```

Normal output:

```
-----BEGIN RSA PUBLIC KEY-----
MIIBI...
... ...
...AQAB
-----END RSA PUBLIC KEY-----
```
Error output:
```
Error in $.e: unrecognized exponent "AQCB"
```
