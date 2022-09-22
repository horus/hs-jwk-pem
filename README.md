# Convert RSA JWK to PEM Format

_Disclaimer: Low quality code for own private purpose only. Use at your own risk._

## Run

As a CLI tool:

```
$ stack run < ./key.json
```

...or as a tiny web service:

```
$ stack run -- web
```

### Docker

For simplicity, one can build local docker images with the given Dockerfile.

```
$ docker build -t hs-jwk-pem .
... ...
$ docker image ls --format "{{.Repository}}:{{.Tag}} {{.Size}}" hs-jwk-pem
hs-jwk-pem:latest 87.5MB

$ docker run -it -p 8080:9000 --rm hs-jwk-pem
```

### Web API

Example request:

```
$ curl \
  --request POST \
  --url http://127.0.0.1:9000/ \
  --header 'Content-Type: application/json' \
  --data '{"kty": "RSA","n": "...","e": "AQAB"}'
```

Normal output (X.509):

```
-----BEGIN PUBLIC KEY-----
MIIBI...
... ...
...AQAB
-----END PUBLIC KEY-----
```

Note: use `http://127.0.0.1:9000/?rsa` to get a PKCS #1-encoded public key.

Error output:
```
Error in $.e: unrecognized exponent "AQCB"
```
