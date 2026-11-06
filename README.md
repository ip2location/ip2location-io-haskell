# IP2Location.io Haskell SDK — IP Geolocation, Domain WHOIS & Hosted Domain Lookup

**IP2Location.io Haskell SDK** (`ip2location-io`) is a Haskell library for retrieving **IPv4 and IPv6 geolocation, network, and IP intelligence data** through the [IP2Location.io API](https://www.ip2location.io/). It also provides **domain WHOIS lookup**, domain/URL helpers, and a documented **hosted-domain lookup** example. The library lets Haskell applications access IP and domain information without manually constructing API requests.

**Quick links:** [Get an API key](https://www.ip2location.io/pricing) · [Developer documentation](https://ip2location-io-haskell.readthedocs.io/en/latest/index.html) · [API reference](https://ip2location-io-haskell.readthedocs.io/en/latest/code.html) · [GitHub repository](https://github.com/ip2location/ip2location-io-haskell)

## Features

- **IPv4 and IPv6 geolocation:** Retrieve country, region, district, city, latitude, longitude, and ZIP/postal code.
- **Network and ASN information:** Access ASN, AS name, ISP, associated domain, connection speed, and usage type when available.
- **Time zone and localization data:** Inspect time zone information, available DST details, and translated location names (Plus and Security plans).
- **Proxy and security signals:** Access plan-dependent proxy, VPN, Tor, data center, crawler, and fraud-related fields.
- **Additional IP intelligence:** Retrieve available mobile carrier data (MCC/MNC), country metadata, advertising categories, and other attributes.
- **Domain WHOIS lookup:** Access domain registration dates, registrar details, nameservers, status, and available contacts.
- **Domain and URL utilities:** Extract domain names from URLs and get domain extensions.
- **Hosted domains:** The repository and Read the Docs include a hosted-domain API example with paginated results; see the package-availability note below.

> **Plan availability:** Fields vary by subscription plan and queried address. Optional values can be absent even when a field is supported. See [API plans](https://www.ip2location.io/pricing) and the [Haskell API reference](https://ip2location-io-haskell.readthedocs.io/en/latest/code.html).

## SDK modules and functions

The [official Haskell API reference](https://ip2location-io-haskell.readthedocs.io/en/latest/code.html) describes these operations:

| Module / function | Description |
| --- | --- |
| `Configuration.open(apiKey)` | Creates the configuration object with the API key. |
| `IPGeolocation.lookUpIP config ip language` | Retrieves geolocation and eligible network/security attributes for an IPv4 or IPv6 address. |
| `DomainWhois.lookUpDomain config domain` | Retrieves WHOIS data for a domain name. |
| `DomainWhois.getDomainName url` | Extracts the domain name from a URL. |
| `DomainWhois.getDomainExtension value` | Retrieves the domain extension from a domain or URL. |
| `HostedDomain.lookUpHosted config ip page` | Retrieves a page of hosted-domain results for an IP address. |

**Module availability note:** As checked in the [GitHub `.cabal` manifest](https://github.com/ip2location/ip2location-io-haskell/blob/main/ip2location-io.cabal), the library's `exposed-modules` list includes `IPGeolocation`, `DomainWhois`, and `Configuration`, but not `HostedDomain`, even though the [documentation](https://ip2location-io-haskell.readthedocs.io/en/latest/quickstart.html) and repository contain `HostedDomain.hs`. Consequently, the hosted-domain import shown below may not be available from the published package as configured. Verify the installed version or consult the repository before using that example.

## API access and query limits

IP2Location.io provides keyless geolocation access through its REST API and a Free plan with an API key:

| API service | Keyless (no API key) | Free plan (with API key) |
| --- | --- | --- |
| IP Geolocation API | **1,000 queries/day** | **50,000 queries/month** |
| Domain WHOIS API | — | **500 queries/month** |
| Hosted Domain API | — | **50 queries/month** |

- **Keyless access:** Limited keyless IP geolocation queries are available directly through the [REST API](https://www.ip2location.io/ip2location-documentation); the Haskell SDK documentation requires an API key and does not describe a keyless SDK mode. The keyless allowance resets at **00:00 UTC**.
- **Free plan:** Create an API key to use the monthly allowances shown above.
- **Attribution:** The keyless and Free offerings require attribution; consult the [pricing page](https://www.ip2location.io/pricing) for current terms.
- **Paid plans:** Higher limits and additional data fields are described on the [pricing page](https://www.ip2location.io/pricing).

For authentication and rate-limit details, see the [IP Geolocation REST API documentation](https://www.ip2location.io/ip2location-documentation).

## Requirements

- A working [Haskell compiler (GHC)](https://www.haskell.org/ghc/) and [Cabal](https://www.haskell.org/cabal/).
- The `ip2location-io` Haskell package and its dependencies, resolved by Cabal. The [package manifest](https://github.com/ip2location/ip2location-io-haskell/blob/main/ip2location-io.cabal) specifies dependencies including `aeson`, `http-client`, and `http-client-tls`.
- An [IP2Location.io API key](https://www.ip2location.io/pricing) for the SDK examples.
- Network connectivity to the relevant IP2Location.io services.

The package manifest specifies the `Haskell2010` language standard and a compatible `base` range. Check the [current manifest](https://github.com/ip2location/ip2location-io-haskell/blob/main/ip2location-io.cabal) for compiler compatibility instead of assuming all GHC releases are supported.

## Installation

The [official Haskell quickstart](https://ip2location-io-haskell.readthedocs.io/en/latest/quickstart.html) documents installation with Cabal:

```bash
cabal install ip2location-io
```

For an application project, declare `ip2location-io` as a dependency in your `.cabal` project configuration so its modules are available when building your program.

For the examples below, use an API key. The official samples place `"YOUR_API_KEY"` directly in the source; to avoid committing a credential, the quickstart below reads it from an environment variable instead.

**Linux / macOS terminal:**

```bash
export IP2LOCATION_API_KEY='YOUR_API_KEY'
```

**Windows Command Prompt:**

```bat
set IP2LOCATION_API_KEY=YOUR_API_KEY
```

## Quickstart: IP geolocation lookup

Create `Main.hs` with a minimal version of the [official IP geolocation example](https://ip2location-io-haskell.readthedocs.io/en/latest/quickstart.html):

```haskell
import Configuration
import qualified IPGeolocation as IPG
import System.Environment (lookupEnv)

main :: IO ()
main = do
    maybeKey <- lookupEnv "IP2LOCATION_API_KEY"
    case maybeKey of
        Nothing -> putStrLn "Set the IP2LOCATION_API_KEY environment variable."
        Just apikey -> do
            config <- open apikey
            result <- IPG.lookUpIP config "8.8.8.8" ""

            case result of
                IPG.IPError err ->
                    putStrLn $ "ERROR: " ++ IPG.error_message (IPG.error err)
                IPG.IPResponse response -> do
                    putStrLn $ "IP: " ++ IPG.ip response
                    putStrLn $ "Country: " ++ IPG.country_name response
                    putStrLn $ "City: " ++ IPG.city_name response
                    putStrLn $ "ASN: " ++ IPG.asn response
```

The empty third argument (`""`) disables the optional translated-location-name request. The SDK returns a value that is handled with `IPG.IPError` or `IPG.IPResponse`, as shown in its official documentation.

Build and run `Main.hs` using your Cabal project setup (with `ip2location-io` declared as a dependency). For example, inside an executable Cabal project:

```bash
cabal run
```

Response attributes and their availability depend on your plan and the IP address. See the [Haskell geolocation field reference](https://ip2location-io-haskell.readthedocs.io/en/latest/code.html).

## Working with Haskell responses

The documented Haskell examples use **pattern matching** to distinguish a successful result from an API error, and accessor functions to read response fields. Some optional values are represented with `Maybe` and can be handled with `case`, `maybe`, or other standard Haskell constructs.

The examples below are individual snippets. Unless otherwise stated, they assume you have imported `Configuration`, created `config` using `open apikey`, and are inside an `IO` action (such as `main`).

## More usage examples

### Request translated location names

The third argument to `IPG.lookUpIP` is a language code. Translations for continent, country, region, and city names are available on **Plus and Security** plans. The official example uses a blank string when translation is not requested.

```haskell
import qualified IPGeolocation as IPG

-- Inside main, after creating config:
result <- IPG.lookUpIP config "8.8.8.8" "es"
case result of
    IPG.IPError err -> putStrLn $ "ERROR: " ++ IPG.error_message (IPG.error err)
    IPG.IPResponse response ->
        putStrLn $ "Country: " ++ IPG.country_name response
```

### Inspect ASN and ISP information

ASN and AS name appear directly on the documented geolocation response. ISP data may be absent under some plans, so use `maybe` for that optional field.

```haskell
import qualified IPGeolocation as IPG

result <- IPG.lookUpIP config "8.8.8.8" ""
case result of
    IPG.IPError err -> putStrLn $ "ERROR: " ++ IPG.error_message (IPG.error err)
    IPG.IPResponse response -> do
        putStrLn $ "ASN: " ++ IPG.asn response
        putStrLn $ "AS: " ++ IPG.as response
        putStrLn $ "ISP: " ++ maybe "NOT_SUPPORTED" id (IPG.isp response)
```

### Inspect available proxy indicators

Security data is plan-dependent. The official geolocation sample checks whether the nested proxy object is present before reading its fields.

```haskell
import qualified IPGeolocation as IPG

result <- IPG.lookUpIP config "8.8.8.8" ""
case result of
    IPG.IPError err -> putStrLn $ "ERROR: " ++ IPG.error_message (IPG.error err)
    IPG.IPResponse response -> do
        putStrLn $ "Is proxy: " ++ show (IPG.is_proxy response)
        case IPG.proxy response of
            Nothing -> putStrLn "Proxy details unavailable."
            Just prx -> do
                putStrLn $ "VPN: " ++ show (IPG.is_vpn prx)
                putStrLn $ "Tor: " ++ show (IPG.is_tor prx)
```

Treat these indicators as supporting signals rather than proof of malicious activity.

### Look up domain WHOIS information

The [official WHOIS example](https://ip2location-io-haskell.readthedocs.io/en/latest/quickstart.html) uses `DomainWhois.lookUpDomain` with the configuration object and domain name:

```haskell
import qualified DomainWhois as DW
import qualified DomainWhois as Regist (Registrar(..))

result <- DW.lookUpDomain config "example.com"
case result of
    DW.WhoisError err -> putStrLn $ "ERROR: " ++ DW.error_message (DW.error err)
    DW.WhoisResponse response -> do
        putStrLn $ "Domain: " ++ DW.domain response
        putStrLn $ "Created: " ++ DW.create_date response
        putStrLn $ "Expires: " ++ DW.expire_date response
        let registrarObj = DW.registrar response
        putStrLn $ "Registrar: " ++ Regist.name registrarObj
```

WHOIS contact details can be unavailable or redacted. Refer to the [WHOIS response field reference](https://ip2location-io-haskell.readthedocs.io/en/latest/code.html) for the fields documented by this SDK.

### Extract a domain name and extension

These utilities follow the official Haskell examples and do not require a network lookup:

```haskell
import qualified DomainWhois as DW

putStrLn $ DW.getDomainName "https://www.example.com/exe"
putStrLn $ DW.getDomainExtension "example.com"
```

The Haskell documentation shows these two utilities; unlike some other language SDKs, it does **not** document Punycode conversion helpers here.

### Find domains hosted on an IP address

The [official hosted-domain example](https://ip2location-io-haskell.readthedocs.io/en/latest/quickstart.html) requests a specific result page, starting at `1`:

```haskell
import qualified HostedDomain as HD

result <- HD.lookUpHosted config "8.8.8.8" 1
case result of
    HD.HostedError err -> putStrLn $ "ERROR: " ++ HD.error_message (HD.error err)
    HD.HostedResponse response -> do
        putStrLn $ "IP: " ++ HD.ip response
        putStrLn $ "Total domains: " ++ show (HD.total_domains response)
        putStrLn $ "Page: " ++ show (HD.page response)
        putStrLn $ "Total pages: " ++ show (HD.total_pages response)
        putStrLn $ "Domains: " ++ show (HD.domains response)
```

Use page `2`, `3`, and so on for additional pages when available. **Packaging caveat:** The current repository `.cabal` file does not expose `HostedDomain` even though the source and documentation include it. Check module availability in your installed version before relying on this snippet.

## Haskell package and module troubleshooting

- **Module cannot be found:** Make sure `ip2location-io` is a declared dependency of the executable you are building. A system-level `cabal install` alone may not make a library import available to every project.
- **`HostedDomain` cannot be imported:** The published manifest's `exposed-modules` entry omits `HostedDomain`. Check the repository and package version; this is a packaging/documentation discrepancy, not an API-key issue.
- **Missing optional values:** The SDK documentation uses `Maybe` for fields such as `isp` and `proxy`. Handle `Nothing` rather than assuming every plan returns every field.
- **API errors:** Handle the `IPError`, `WhoisError`, and `HostedError` constructors shown in the official examples, and check your API key, quota, and requested service.

## Common use cases

- **Application localization:** Adapt regional content or preferred-language suggestions using approximate IP location.
- **Network and traffic analysis:** Attach country, ASN, and available ISP attributes to server/application events.
- **Security review:** Use eligible proxy, VPN, Tor, and fraud indicators as part of broader risk assessment.
- **Domain research:** Review WHOIS registration dates, registrars, domain status, and nameservers.
- **Hosting research:** Investigate domains associated with an IP address where the hosted-domain module is available.

## Documentation and support

- [Haskell SDK quickstart and official sample codes](https://ip2location-io-haskell.readthedocs.io/en/latest/quickstart.html)
- [Haskell SDK API reference and response fields](https://ip2location-io-haskell.readthedocs.io/en/latest/code.html)
- [Official Haskell GitHub repository](https://github.com/ip2location/ip2location-io-haskell)
- [Haskell package manifest and dependencies](https://github.com/ip2location/ip2location-io-haskell/blob/main/ip2location-io.cabal)
- [IP2Location.io API plans and query limits](https://www.ip2location.io/pricing)
- [IP Geolocation API authentication and rate limits](https://www.ip2location.io/ip2location-documentation)
- [Report an SDK issue or request a feature](https://github.com/ip2location/ip2location-io-haskell/issues)
- [API and account support](https://www.ip2location.io/contact-us)

## License

Licensed under the **MIT License**. See [LICENSE](./LICENSE).
