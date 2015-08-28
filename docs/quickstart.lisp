* (ql:quickload :cl-cfssl-client)
To load "cl-cfssl-client":
  Load 1 ASDF system:
    cl-cfssl-client
; Loading "cl-cfssl-client"
.......
(:CL-CFSSL-CLIENT)
* (defvar *local-server* (cfssl-client:new-server "127.0.0.1"))

*LOCAL-SERVER*
* (defvar *remote-server* (cfssl-client:new-server "ca.example.org"))

*REMOTE-SERVER*
* (defvar *certificate-request*
     (cfssl-client:new-certificate-request '("www.example.org" "mail.example.org" "example.org")
                                           (cfssl-client:new-subject-name :country "US" :org "Example Organisation")
                                           :common-name "example.org" :key-spec cfssl-client:*rsa-2048-key*))

*CERTIFICATE-REQUEST*
* (defvar *key-and-csr* (cfssl-client:new-key-and-csr *local-server* *certificate-request*))

*KEY-AND-CSR*
* (type-of *key-and-csr*)

HASH-TABLE
* (gethash "certificate-request" *key-and-csr*)

"-----BEGIN CERTIFICATE REQUEST-----
MIIC0zCCAbsCAQAwQjELMAkGA1UEBhMCVVMxHTAbBgNVBAoTFEV4YW1wbGUgT3Jn
YW5pc2F0aW9uMRQwEgYDVQQDEwtleGFtcGxlLm9yZzCCASIwDQYJKoZIhvcNAQEB
BQADggEPADCCAQoCggEBAN0YAlu0E6XuY49bzs+u709C559/kJrEuiYPVVCR+wW5
YE0rdBocNSCKQdBNODEpcsyybWosZ/JY6U/mpUQxN43B3aNb12QctsCBMvcmTUuy
vg39tK8GMf3/oUNjOQtVDyG5TKg3wYEyD9IlkvwhonKaPB3cwXhI3XAbpJTK3evf
3fcTHTt4FOzYXUSi6PnF+ECCP2XEmfFECBv/E+vzfvha5x6RKmuUgZ3nCYfzcpX0
yD7pznxMv9cC5kVbvW6w6UWm7TtHJhoddRLbJeoZz2ONvtO9BK+kWg74BjyhHI0/
zJw+mA7ssTYhoEsQHEe3VJ03eYfE4lcjuGjrX/3Uku0CAwEAAaBMMEoGCSqGSIb3
DQEJDjE9MDswOQYDVR0RBDIwMIIPd3d3LmV4YW1wbGUub3JnghBtYWlsLmV4YW1w
bGUub3JnggtleGFtcGxlLm9yZzANBgkqhkiG9w0BAQsFAAOCAQEAKlPWv+TLerKW
rqtK5kAu1eLBPZul+F9NShfG2ETRufKlljpD2JeJae+P7Vi86eKS+Y8ec9hPQiOC
/cZM9vPmP92ysFVy5iLaQCtxrQb9ZXb/aIR0XW6xbg51+OTPTcuPTcLgw1pYYrwH
oRRbdTUZhivCJJB7i4tVmivvm4zDUvD8dy4MWvd7K/nMKtLlVFlBFRSHbOCaIX0D
+fcrs/1LMzqPCzdwQMZnS2ZfXgq+VLNmoIFZJT5Fps1IQCJcr4cghzW2UgTAv6X/
dPitnh8aGs38sjypHqsXtV+TENghjylL+n/3TBgK3jKb0qwZo3ifw0/rrLCGS8Qp
r6N9OVamiw==
-----END CERTIFICATE REQUEST-----
"
T
* (defvar *signing-request*
     (cfssl-client:new-sign-request (cfssl-client:hosts-of *certificate-request*)
                                    (gethash "certificate-request" *key-and-csr*)
                                    (cfssl-client:subject-from-certificate-request *certificate-request*)))

*SIGNING-REQUEST*
* (defvar *certificate* (cfssl-client:sign *remote-server* *signing-request*))

*CERTIFICATE*
* (type-of *certificate*)

(VECTOR CHARACTER 1343)
* *certificate*

"-----BEGIN CERTIFICATE-----
MIIDZzCCAk+gAwIBAgIIXd78b5jpUMMwDQYJKoZIhvcNAQELBQAwEjEQMA4GA1UE
ChMHQWNtZSBDbzAeFw0xNTA4MjgwNDI0MDBaFw0xNjA3MjYxMjI0MDBaMEIxCzAJ
BgNVBAYTAlVTMR0wGwYDVQQKExRFeGFtcGxlIE9yZ2FuaXNhdGlvbjEUMBIGA1UE
AxMLZXhhbXBsZS5vcmcwggEiMA0GCSqGSIb3DQEBAQUAA4IBDwAwggEKAoIBAQDd
GAJbtBOl7mOPW87Pru9PQueff5CaxLomD1VQkfsFuWBNK3QaHDUgikHQTTgxKXLM
sm1qLGfyWOlP5qVEMTeNwd2jW9dkHLbAgTL3Jk1Lsr4N/bSvBjH9/6FDYzkLVQ8h
uUyoN8GBMg/SJZL8IaJymjwd3MF4SN1wG6SUyt3r3933Ex07eBTs2F1Eouj5xfhA
gj9lxJnxRAgb/xPr8374WucekSprlIGd5wmH83KV9Mg+6c58TL/XAuZFW71usOlF
pu07RyYaHXUS2yXqGc9jjb7TvQSvpFoO+AY8oRyNP8ycPpgO7LE2IaBLEBxHt1Sd
N3mHxOJXI7ho61/91JLtAgMBAAGjgZAwgY0wDgYDVR0PAQH/BAQDAgeAMBMGA1Ud
JQQMMAoGCCsGAQUFBwMEMAwGA1UdEwEB/wQCMAAwHQYDVR0OBBYEFEyG+DVtqsTs
l+ss5SolslyhUSB+MDkGA1UdEQQyMDCCD3d3dy5leGFtcGxlLm9yZ4IQbWFpbC5l
eGFtcGxlLm9yZ4ILZXhhbXBsZS5vcmcwDQYJKoZIhvcNAQELBQADggEBAFlqr+eb
/t9o/18aERpilo0X5ZSVTlcpBF6bVXgqAFUYUpftsoMDhfMfh4fCZNUnGEXMZdgj
stvcSFE9neHvjLvnKZzbta6Qa77mw7SBiYZMGV94dnsoUj1R7Mr2+VNNRI69N9go
ZjXszGUGx1jOIm/ZFuRRatJP5k7thDCYEMpFXQRaRQKCmJZyiVtlni+SGzww8Wm2
5Hh4x91LEVyPtL1AYdsv1cizxYdM0EjAxV7MhPTClzyb1C7YNA8fiEoC99K9GOYS
SQIwRLU+lB6A/CASPDR96an23ZNjmw540flv7sFZ3qh7VpglxPRuHMXEkxdCTgvT
xF+o4Rvwb6KSgUY=
-----END CERTIFICATE-----
"
* (defvar *remote-ca-cert* (cfssl-client:info *remote-server*))

*REMOTE-CA-CERT*
* (type-of *remote-ca-cert*)

(VECTOR CHARACTER 1343)
* *remote-ca-cert*

"-----BEGIN CERTIFICATE-----
MIIC+DCCAeCgAwIBAgIRAMBaxaxyYGr7RAXYgOqFIu4wDQYJKoZIhvcNAQELBQAw
EjEQMA4GA1UEChMHQWNtZSBDbzAeFw0xNTA4MjcwNjE5MzlaFw0xNjA4MjYwNjE5
MzlaMBIxEDAOBgNVBAoTB0FjbWUgQ28wggEiMA0GCSqGSIb3DQEBAQUAA4IBDwAw
ggEKAoIBAQCig1dh15u0M3mfsVvPnXFxc7bZs1umisQvkRMcaiINL/DL2rAzryLH
mB5KiV5wCPO6EPhYj101FxGNMyamFmbyfkFDhJB912Wxh8bivuiav0rto0j8aXBP
KA+vGlUGtR0O5Lt4CHs2PHwFWSAov3msXn4roSMpjatOzWh+RhT4IYN4IfFkXTyU
1xc6MREPHCknG+cNpRtWuGlq4iHEQvMigOKQh5PwSurNs+wiGnx8X/Inhrr9tT3l
r7ZeuCaUyuAoAGoM04GD1CD96H1NS+BbAWRyY1t1FioUct4/u62qy3AZ8NB1seqt
/fy9o+8DGF5QLQtJN4D7bwFlKd9KTJAJAgMBAAGjSTBHMA4GA1UdDwEB/wQEAwIF
oDATBgNVHSUEDDAKBggrBgEFBQcDATAMBgNVHRMBAf8EAjAAMBIGA1UdEQQLMAmC
B3Rlc3QtY2EwDQYJKoZIhvcNAQELBQADggEBAEepBnZM85m3nr5EEjdSIpYoQeDN
e2F/FVNjPRe2RuAx6B+spYiHTg6sCfcj0r/KZfa3DC2+Djlil++9VTRni4orO+PM
8bplzWj/A4kGBf5GC3+C6ihGUpqhwALT9KZX3BFs5r+MjrLv3DFGpDVxAQxccspd
z42x2gDhcSKkoS1WQ7P4oOg+hQRcGz2fFoD75nLOiOCTjcB9j873MAXBHfUXwQaq
mlE0w3KHUdhjOmvFBeHkUzBvJ5giZmUK58rCxn8LzQh4CSUdYp75hdF80IJwtTgs
4PrQP1VOHNSe7aOTbnyP2jNmPWePy3MAQT1ClnMbIBcGEoUN3yIPCVLJ1No=
-----END CERTIFICATE-----"
* (defvar *remote-info* (cfssl-client:info *remote-server* :extra t))

*REMOTE-INFO*
* (type-of *remote-info*)

HASH-TABLE
* (cfssl-client:hash-table-keys *remote-info*)

("expiry" "usages" "certificate")
* (maphash (lambda (k v)
              (format t "~A: ~A~%" k v))
           *remote-info*)
certificate: -----BEGIN CERTIFICATE-----
MIIC+DCCAeCgAwIBAgIRAMBaxaxyYGr7RAXYgOqFIu4wDQYJKoZIhvcNAQELBQAw
EjEQMA4GA1UEChMHQWNtZSBDbzAeFw0xNTA4MjcwNjE5MzlaFw0xNjA4MjYwNjE5
MzlaMBIxEDAOBgNVBAoTB0FjbWUgQ28wggEiMA0GCSqGSIb3DQEBAQUAA4IBDwAw
ggEKAoIBAQCig1dh15u0M3mfsVvPnXFxc7bZs1umisQvkRMcaiINL/DL2rAzryLH
mB5KiV5wCPO6EPhYj101FxGNMyamFmbyfkFDhJB912Wxh8bivuiav0rto0j8aXBP
KA+vGlUGtR0O5Lt4CHs2PHwFWSAov3msXn4roSMpjatOzWh+RhT4IYN4IfFkXTyU
1xc6MREPHCknG+cNpRtWuGlq4iHEQvMigOKQh5PwSurNs+wiGnx8X/Inhrr9tT3l
r7ZeuCaUyuAoAGoM04GD1CD96H1NS+BbAWRyY1t1FioUct4/u62qy3AZ8NB1seqt
/fy9o+8DGF5QLQtJN4D7bwFlKd9KTJAJAgMBAAGjSTBHMA4GA1UdDwEB/wQEAwIF
oDATBgNVHSUEDDAKBggrBgEFBQcDATAMBgNVHRMBAf8EAjAAMBIGA1UdEQQLMAmC
B3Rlc3QtY2EwDQYJKoZIhvcNAQELBQADggEBAEepBnZM85m3nr5EEjdSIpYoQeDN
e2F/FVNjPRe2RuAx6B+spYiHTg6sCfcj0r/KZfa3DC2+Djlil++9VTRni4orO+PM
8bplzWj/A4kGBf5GC3+C6ihGUpqhwALT9KZX3BFs5r+MjrLv3DFGpDVxAQxccspd
z42x2gDhcSKkoS1WQ7P4oOg+hQRcGz2fFoD75nLOiOCTjcB9j873MAXBHfUXwQaq
mlE0w3KHUdhjOmvFBeHkUzBvJ5giZmUK58rCxn8LzQh4CSUdYp75hdF80IJwtTgs
4PrQP1VOHNSe7aOTbnyP2jNmPWePy3MAQT1ClnMbIBcGEoUN3yIPCVLJ1No=
-----END CERTIFICATE-----
usages: (digital signature email protection)
expiry: 8000h
NIL
