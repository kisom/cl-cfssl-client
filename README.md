# :cfssl-client

This is a Common Lisp package for interacting with
[CFSSL API servers](https://github.com/cloudflare/cfssl/). It
currently implements the functionality from the
`[api/client](http://godoc.org/github.com/cloudflare/cfssl/api/client)`
package. This provides the following features:

+ Getting information from a CFSSL API server
+ Requesting a signed certificate from the server
+ Requesting a signed certificate from the server with the standard
  HMAC authentication
+ Performing these three operations against a single server or a
  fallback list of servers.

The CFSSL remote model presupposes that one has a private key and PKCS
\#10 certificate request already, i.e. via the `/api/v1/cfssl/newkey`
endpoint.

Any and all review/feedback is welcomed.

## The API

### The Remote interface

The CFSSL client API defines a `Remote` interface with three methods:

+ `sign` (request a signed certificate)
+ `info` (request information about a server)
+ `authsign` (request a signed certificate using authentication)

### Connections

There are two types of connections: a single server as represented by
the `server` class, and a group of servers as represented by
`server-group`.

A single server connection can be set up using `#'new-server`. It
takes a "host:port" specification; if the port is omitted, the default
CFSSL port is used. The following both point to the same server:

```
(new-server "127.0.0.1")
(new-server "127.0.0.1:8888")
```

A server group is set up using `#'new-server-group`; it takes a list of
server specifications as its constructor. For example,

```
(new-server-group "ca1.example.org"
				  "ca2.example.org"
				  "ca3.example.org:8080")
```

Either of these types of connections can be used in the three methods mentioned previously.

### Building a request

A CFSSL signature request contains information about the request as
well as a PKCS #10 certificate signing request (CSR). There are a few
functions to assist with this:

+ `#'new-simple-subject` returns a subject information field with a
  single set of names.
+ `#'new-sign-request` returns a signing request that can be given to
  `#'sign` or `#'auth-sign` to obtain a certificate; the CSR should
  given as a string.
+ `#'new-sign-request-from-file` loads the CSR from a file and calls
  `#'new-sign-request` with this CSR.

### Authentication

CFSSL signing endpoints can also be
[authenticated](http://godoc.org/github.com/cloudflare/cfssl/auth).
This package implements half of the `Provider` interface, namely that
part concerned with generating tokens, using the same `Standard`
provider.

A standard provider is constructed using `#'new-standard-auth`,
supplied with a hex-encoded key:

```
(new-standard-auth
  "2cd9f6e973af3030c560ae29066560d297d6730a944f9b803be114650faedefb")
```

The authenticated signing method `#'auth-sign` takes a connection (a
`server` or `server-group`), a request (either a `sign-request` or an
`auth-sign-request`), and an optional provider. If the request is a
`sign-request`, the provider will be used to generate an authenticated
request.

**N.B.**: for API compatibility, there is a fourth optional parameter,
`id`; this isn't used in CFSSL right now and is ignored here.

## Examples

### Building a request and signing with a single server

```
(let ((request (new-sign-request-from-file
		'("www.example.org" "mail.example.org" "example.org")
		#P"/tmp/cert.csr"
		(new-simple-subject
		 :common-name "example.org"
		 :country "US"
		 :state "California"
		 :org "Example Org"
		 :org-unit "Operations")))
      (server (new-server "127.0.0.1")))
  (sign server request))
```

### Authenticated signing

```
(let ((request (new-sign-request-from-file
		'("www.example.org" "mail.example.org" "example.org")
		#P"/tmp/cert.csr"
		(new-simple-subject
		 :common-name "example.org"
		 :country "US"
		 :state "California"
		 :org "Example Org"
		 :org-unit "Operations")))
      (auth (new-standard-auth "0123456789abcdef0123456789abcdef"))
      (server (new-server "127.0.0.1")))
  (auth-sign server request auth))
```

## License

Licensed under the MIT license.

